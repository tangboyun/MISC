{-# LANGUAGE BangPatterns,RankNTypes,FlexibleContexts,TypeFamilies,GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2013 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference: A Permutation Approach to Testing Interactions in Many Dimensions
--            Noah Simon, Robert Tibshirani
-- 
--
-----------------------------------------------------------------------------

module Bio.Statistics.Interaction.TMIcor
       
       where
import           Bio.Statistics.Util
import           Control.Exception (assert)
import           Control.Monad
import           Control.Monad.Par
import           Control.Monad.Primitive
import           Control.Monad.ST.Strict
import           Control.Parallel.Strategies hiding (parMap)
import           Data.Foldable (toList)
import           Data.Function
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as B
import qualified Data.Heap as Heap
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           Debug.Trace (trace)
import           GHC.Conc (numCapabilities)
import           Statistics.Sample
import           System.Random.MWC

type HashTable s k v = B.HashTable s k v
-- | Result: GeneIdx1, GeneIdx2, TMIcor-value, p-value, FDR
type Result = UV.Vector (Int,Int,Double,Double,Double)


data GData v where
  GD :: GV.Vector v Double =>
        { pVariable :: {-# UNPACK #-} !Int -- p >> n
        , nSample :: {-# UNPACK #-} !Int
        , dat :: v Double
        } -> GData v

toStdScores :: (GV.Vector v Double, NFData (v Double)) => Int -> [v Double] -> [v Double]
toStdScores nTrue =
--  withStrategy (parListChunk (halfL1 `quot` (doubleSize*2*nTrue)) rdeepseq) .
  map
  (\v -> runST $ do
      let n = GV.length v
          nFalse = n - nTrue
          v1 = GV.slice 0 nTrue v
          v2 = GV.slice nTrue nFalse v 
          (m1,var1) = meanVarianceUnb v1 
          (m2,var2) = meanVarianceUnb v2
      mv <- GV.unsafeThaw v
      forM_ [0..n-1] $ \idx -> do
        e <- GMV.unsafeRead mv idx
        if idx < nTrue
          then GMV.unsafeWrite mv idx ((e-m1) / sqrt var1)
          else GMV.unsafeWrite mv idx ((e-m2) / sqrt var2)
      GV.unsafeFreeze mv)
  where
    halfL1 = 32 * 1024
    doubleSize = 8

calcIdx :: UV.Vector Int -> Int -> Int -> (Int,Int)
calcIdx vec n idx =
  let i = getI
      j = getJ i idx
  in (i,j)
  where
    getJ i p = (2*p+i*i+3*i-2*i*n+2) `quot` 2
    getI = go 0
      where
        go acc = if UV.unsafeIndex vec (acc+1) > idx
                 then acc
                 else go (acc+1)
                      
getVec :: Int -> UV.Vector Int
getVec n = UV.generate (n-1) (\i -> (2*i*n-i*i-i) `quot` 2)  

tmiCorV3 :: (GV.Vector v1 Double,GV.Vector v2 Bool,v1~v2)
       => Seed -- ^ initial random seed
       -> Int  -- ^ repeat times for permutation
       -> Int  -- ^ first k gene pairs
       -> GData v1 -- ^ vector of gene chip
       -> v2 Bool
       -> (Result,[Double])
--       -> (Result,Seed)
{-# INLINABLE tmiCorV3 #-}       
tmiCorV3 seed nPerm kPairs (GD p n _data) label' = assert (n == GV.length label') $
  let nTrue = count True label'
      nFalse = n - nTrue
      label = UV.fromList $ replicate nTrue True ++ 
              replicate nFalse False
      vVec = V.fromList $ toStdScores nTrue $ toSlices n $ recomb n label' _data
      hs = kLargestV2 nTrue kPairs vVec
      (gps,tCors) = unzip hs
      gpVec = UV.fromList gps
      tCorVec = UV.fromList tCors
      (ls,seed') = permute seed nPerm $ UV.enumFromN 0 n
      chunkSize = 5000
      minCor = abs $ head tCors
      psVec = getVec p
  in
   runST $ do
     tmv <- UV.unsafeThaw $ UV.replicate kPairs 0
     forM_ ls $ \permVec -> do
       let vVec' = withStrategy rdeepseq $ V.fromList $
                   toStdScores nTrue $
                   toSlices n $
                   UV.generate (n*p) $
                     (\idx ->
                       let (i,j) = idx `divMod` n
                           vec = V.unsafeIndex vVec i
                       in GV.unsafeIndex vec (GV.unsafeIndex permVec j))
           t = p * (p-1) `quot` 2
           onCore vV i = runST $ do
             mv <- UV.unsafeThaw $ UV.replicate kPairs 0
             let idxs = [i,i + numCapabilities..t-1]
             mapM_ (collect mv tCorVec .
                    dropWhile (<= minCor) . sort .
                    map
                    (\(i,j) -> 
                      abs $
                      tCorStdedV2 nTrue (V.unsafeIndex vV i) (V.unsafeIndex vV j) 
                    )) . chunksOf 5000 . map (calcIdx psVec p) $ idxs
             UV.unsafeFreeze mv
           rVec = foldl1 (UV.zipWith (+)) $ runPar $ parMap (onCore vVec') [0..numCapabilities - 1]
           
       forM_ [0..kPairs-1] $ \idx -> do
         e <- UMV.unsafeRead tmv idx
         UMV.unsafeWrite tmv idx (e + UV.unsafeIndex rVec idx)
         
     uv <- UV.unsafeFreeze tmv
     let csum = UV.postscanr' (+) 0 uv      
         fdrVec = trace (show $ UV.sum uv) $
                  UV.imap
                  (\i e ->
                    fromIntegral e /
                    fromIntegral ((kPairs-i)*nPerm)) csum
         pVec = UV.map ((/ (0.5 * (fromIntegral (nPerm * p * (p-1))))) . fromIntegral) csum
     result <- liftM UV.reverse $ UV.generateM kPairs $ \k -> 
       let idx@(i,j) = UV.unsafeIndex gpVec k
           cor = UV.unsafeIndex tCorVec k
           fdr = UV.unsafeIndex fdrVec k
           p = UV.unsafeIndex pVec k
       in return (i,j,cor,p,fdr)
     return (result,tCors)



count :: (GV.Vector v a, Eq a) => a -> v a -> Int
count e vec = go 0 0
  where
    n = GV.length vec
    go acc idx | idx < n, e == GV.unsafeIndex vec idx = go (acc+1) (idx+1)
               | idx < n = go acc (idx+1)
               | otherwise = acc

recomb :: (GV.Vector v1 Double,GV.Vector v2 Bool)
       => Int
       -> v2 Bool
       -> v1 Double -> UV.Vector Double
recomb rowLen label dat =
  let n = count True label
      l = GV.length dat
      (p,r) = l `divMod` rowLen
  in if r /= 0
     then error "recomb: length dat / n ~= 0"
     else runST $ do
       mv <- UMV.new l
       forM_ [0..p-1] $ \i ->
         let tBeg = i * rowLen
             fBeg = tBeg + n
         in GV.foldM_
            (\(tAcc,fAcc) e ->
              let origE = GV.unsafeIndex dat $! i * rowLen + tAcc + fAcc
              in if e
                 then UMV.unsafeWrite mv (tBeg+tAcc) origE >>
                      return (tAcc+1,fAcc)
                 else UMV.unsafeWrite mv (fBeg+fAcc) origE >>
                      return (tAcc,fAcc+1)
              ) (0,0) label
       UV.unsafeFreeze mv
       
newtype TCor = TCor
  { unTCor :: ((Int,Int),Double) } deriving (Eq)

instance Ord TCor where
  -- comparing the abs value
  compare = compare `on` (abs.snd.unTCor)

toSlices :: GV.Vector v Double => Int -> v Double -> [v Double]
toSlices !n !v =
  let (p,r) = GV.length v `divMod` n
  in if r /= 0
     then error "toSlices: length v / n ~= 0"
     else map ((\beg -> GV.slice beg n v).(*n)) [0..p-1]
{-# INLINABLE toSlices #-}

toIdx :: Int -> (Int,Int) -> Int
toIdx rowLen (i,j) = i * rowLen + j


tmiCorV2 :: (GV.Vector v1 Double,GV.Vector v2 Bool,v1~v2)
       => Seed -- ^ initial random seed
       -> Int  -- ^ repeat times for permutation
       -> Int  -- ^ first k gene pairs
       -> GData v1 -- ^ vector of gene chip
       -> v2 Bool
       -> (Result,[Double])
{-# INLINABLE tmiCorV2 #-}       
tmiCorV2 seed nPerm kPairs (GD p n _data) label' = assert (n == GV.length label') $
  let nTrue = count True label'
      nFalse = n - nTrue
      label = UV.fromList $ replicate nTrue True ++ 
              replicate nFalse False
      vVec = V.fromList $
             map
             (\v -> runST $ do
                 let v1 = UV.slice 0 nTrue v
                     v2 = UV.slice nTrue nFalse v 
                     (m1,var1) = meanVarianceUnb v1 
                     (m2,var2) = meanVarianceUnb v2
                 mv <- UV.unsafeThaw v
                 forM_ [0..n-1] $ \idx -> do
                   e <- UMV.unsafeRead mv idx
                   if idx < nTrue
                     then UMV.unsafeWrite mv idx ((e-m1) / sqrt var1)
                     else UMV.unsafeWrite mv idx ((e-m2) / sqrt var2)
                 UV.unsafeFreeze mv
             ) $ toSlices n $ recomb n label' _data
      hs = kLargestV2 nTrue kPairs vVec
      (gps,tCors) = unzip hs
      gpVec = UV.fromList gps
      tCorVec = UV.fromList tCors
      (ls,seed') = permute seed nPerm $ UV.enumFromN 0 n
      minCor = abs $ head tCors
  in
   runST $ do
     umv <- UV.unsafeThaw $ UV.replicate kPairs 0
     forM_ ls $ \permVec -> do
       let vVec' = V.fromList $
                   toSlices n $
--                   recomb n (GV.unsafeBackpermute label permVec) $
                   UV.generate (n*p) $
                     (\idx ->
                       let (i,j) = idx `divMod` n
                           vec = V.unsafeIndex vVec i
                       in GV.unsafeIndex vec (GV.unsafeIndex permVec j))
       mapM_ (collect umv tCorVec) $
         withStrategy (parBuffer numCapabilities rdeepseq) $
         map (\vs ->
               dropWhile (<= minCor) $ sort $
               map
               (\(i,j) ->
                 abs $
                 tCorV2 nTrue (V.unsafeIndex vVec' i) (V.unsafeIndex vVec' j) 
               ) vs    
             ) $ chunksOf 1000 $ [(i,j) | i <- [0..p-2], j <- [i+1..p-1]]

     uv <- UV.unsafeFreeze umv

     let csum = UV.postscanr' (+) 0 uv      
         fdrVec = trace (show $ UV.sum uv) $ UV.imap
                  (\i e ->
                    fromIntegral e /
                    fromIntegral ((kPairs-i)*nPerm)) csum
         pVec = UV.map ((/ (0.5 * (fromIntegral (nPerm * p * (p-1))))) . fromIntegral) csum
     result <- liftM UV.reverse $ UV.generateM kPairs $ \k -> 
       let idx@(i,j) = UV.unsafeIndex gpVec k
           cor = UV.unsafeIndex tCorVec k
           fdr = UV.unsafeIndex fdrVec k
           p = UV.unsafeIndex pVec k
       in return (i,j,cor,p,fdr)
     return (result,tCors)

kLargestV2 :: GV.Vector v Double
           => Int
           -> Int   
           -> V.Vector (v Double)
           -> [((Int,Int),Double)]
{-# INLINABLE kLargestV2 #-}
kLargestV2 nTrue k vVec =
  assert (not . V.null $ vVec) $
  let p = V.length vVec
  in go 0 Heap.empty $
     map
     (\ idx@(i, j) ->
       let v = tCorStdedV2 nTrue (V.unsafeIndex vVec i) (V.unsafeIndex vVec j)
       in TCor (idx, v))
     [(i, j) | i <- [0 .. p - 2], j <- [i + 1 .. p - 1]]
  where go _ h [] = map unTCor $ toList h
        go acc h (x : xs)
          | acc < k = go (acc + 1) (Heap.insert x h) xs
          | otherwise = go (acc + 1) (Heap.insert x $ Heap.deleteMin h) xs


kLargest :: (GV.Vector v1 Double
         ,GV.Vector v2 Bool, v1 ~ v2)
       => Int
       -> V.Vector (v1 Double)
       -> v2 Bool
       -> [((Int,Int),Double)]
{-# INLINABLE kLargest #-}
kLargest k vVec label
  | V.null vVec || GV.null label = error "firstK: empty input"
  | GV.length (V.unsafeIndex vVec 0) /= GV.length label =
    error "firstK: sample num /= label length"
  | otherwise =
    let p = V.length vVec
    in
     go 0 Heap.empty $
     map
     (\ idx@(i, j) ->
       let v = tCorStded (V.unsafeIndex vVec i) (V.unsafeIndex vVec j)
               label
       in TCor (idx, v))
     [(i, j) | i <- [0 .. p - 2], j <- [i + 1 .. p - 1]]
  where go _ h [] = map unTCor $ toList h
        go acc h (x : xs)
          | acc < k = go (acc + 1) (Heap.insert x h) xs
          | otherwise = go (acc + 1) (Heap.insert x $ Heap.deleteMin h) xs




tmiCorV1 :: (GV.Vector v1 Double,GV.Vector v2 Bool,v1~v2)
       => Seed -- ^ initial random seed
       -> Int  -- ^ repeat times for permutation
       -> Int  -- ^ first k gene pairs
       -> GData v1 -- ^ vector of gene chip
       -> v2 Bool
       -> (Result,Seed)
{-# INLINABLE tmiCorV1 #-}       
tmiCorV1 seed nPerm kPairs (GD p n _data) label' =
  let n1 = GV.foldl' (\acc e -> if e then acc + 1 else acc) 0 label'
      label = GV.replicate n1 True GV.++ 
              GV.replicate (GV.length label' - n1) False
      vec = GV.concat $
            map
            (\v ->
              let (v1,v2) = ipartition' v label'
                  (m1,var1) = meanVarianceUnb v1 
                  (m2,var2) = meanVarianceUnb v2
              in (GV.++)
                 (GV.map (\e -> (e - m1) / sqrt var1) v1)
                 (GV.map (\e -> (e - m2) / sqrt var2) v2)                 
            ) $ toSlices n _data
      vVec = V.fromList $ toSlices n vec
      hs = kLargest kPairs vVec label
      (gps,tCors) = unzip hs
      gpVec = UV.fromList gps
      tCorVec = UV.fromList tCors
      (ls,seed') = permute seed nPerm label
      minCor = abs $ head tCors
  in
   runST $ do
     hTable <- H.fromList hs :: ST s (HashTable s (Int,Int) Double)
     -- for calculating p-value
     pTable <- H.newSized kPairs :: ST s (HashTable s (Int,Int) Double)
     -- for calculating FDR
     umv <- UV.unsafeThaw $ UV.replicate kPairs 0
     forM_ [(i,j) | i <- [0..p-2], j <- [i+1..p-1]] $ \idx@(i,j) ->
       let vs = sort $
                withStrategy
                (parList rdeepseq) $
                map
                (abs . tCor (V.unsafeIndex vVec i)
                 (V.unsafeIndex vVec j) . UV.convert) ls
           vs' = dropWhile (<= minCor) vs
       in do
         ex <- H.lookup hTable idx
         when (isJust ex) $
           H.insert pTable idx $ -- p-value
           fromIntegral
           (length $
            dropWhile (<= abs (fromJust ex)) vs) /
           fromIntegral nPerm
         collect umv tCorVec vs'
     uv <- UV.unsafeFreeze umv
     let fdrVec = UV.imap (\i e -> fromIntegral e / fromIntegral ((kPairs-i)*nPerm)) $
                  UV.postscanr' (+) 0 uv      
     result <- liftM UV.reverse $ UV.generateM kPairs $ \k -> 
       let idx@(i,j) = UV.unsafeIndex gpVec k
           cor = UV.unsafeIndex tCorVec k
           fdr = UV.unsafeIndex fdrVec k
       in do
        pValue <- liftM fromJust $ H.lookup pTable idx 
        return (i,j,cor,pValue,fdr)
     return (result,seed')

     
collect :: (PrimMonad m, GMV.MVector v Int) => v (PrimState m) Int -> UV.Vector Double -> [Double] -> m ()
{-# INLINABLE collect #-}
collect gmv tCorVec = go 1
  where
    go idx vs | idx == UV.length tCorVec = do
      n <- GMV.unsafeRead gmv (idx-1)
      GMV.unsafeWrite gmv (idx-1) (n + length vs)
              | otherwise =
      let (vs1,vs2) = break (> abs (UV.unsafeIndex tCorVec idx)) vs
      in do
        n <- GMV.unsafeRead gmv (idx-1)
        GMV.unsafeWrite gmv (idx-1) (n + length vs1)
        go (idx+1) vs2


tCorStdedV2 :: (GV.Vector v1 Double
            ,GV.Vector v2 Double
            ,v1 ~ v2)
          => Int -> v1 Double -> v2 Double -> Double
{-# INLINABLE tCorStdedV2 #-}             
tCorStdedV2 nTrue vi vj =
  let (v1i,v2i) = GV.splitAt nTrue vi
      (v1j,v2j) = GV.splitAt nTrue vj
  in atanh (pccStded v1i v1j) -
     atanh (pccStded v2i v2j)

tCorV2 :: (GV.Vector v1 Double
       ,GV.Vector v2 Double
       ,v1 ~ v2)
     => Int -> v1 Double -> v2 Double -> Double
{-# INLINABLE tCorV2 #-}        
tCorV2 nTrue vi vj =
  let (v1i,v2i) = GV.splitAt nTrue vi
      (v1j,v2j) = GV.splitAt nTrue vj
  in atanh (pcc v1i v1j) -
     atanh (pcc v2i v2j)

tCorStded :: (GV.Vector v1 Double
            ,GV.Vector v2 Double
            ,GV.Vector v3 Bool
            ,v1 ~ v2,v1 ~ v3)
          => v1 Double -> v2 Double -> v3 Bool -> Double
{-# INLINABLE tCorStded #-}             
tCorStded vi vj label =
  let (v1i,v2i) = ipartition' vi label
      (v1j,v2j) = ipartition' vj label
  in atanh (pccStded v1i v1j) -
     atanh (pccStded v2i v2j)
            

tCor :: (GV.Vector v1 Double
       ,GV.Vector v2 Double
       ,GV.Vector v3 Bool
       ,v1 ~ v2,v1 ~ v3)
     => v1 Double -> v2 Double -> v3 Bool -> Double
{-# INLINABLE tCor #-}        
tCor vi vj label =
  let (v1i,v2i) = ipartition' vi label
      (v1j,v2j) = ipartition' vj label
  in atanh (pcc v1i v1j) -
     atanh (pcc v2i v2j)
        


pccStded :: (GV.Vector v1 Double
           ,GV.Vector v2 Double
           ,v1 ~ v2)
         => v1 Double
         -> v2 Double
         -> Double
{-# INLINABLE pccStded #-}         
pccStded v1 v2 | GV.length v1 == GV.length v2 = 
  (/ fromIntegral (GV.length v1 + 1)) $
  GV.foldl1' (+) $ GV.zipWith (*) v1 v2
               | otherwise = error "pccStded: input vec did not have equal length."
            
