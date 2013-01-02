{-# LANGUAGE BangPatterns,RankNTypes,FlexibleContexts,TypeFamilies #-}
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

module TMIcor
       (
         Result(..)
       , GData(..)
       , tmiCor
       )
       
       where
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST.Strict
import           Control.Parallel.Strategies
import           Data.Foldable (toList)
import           Data.Function
import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as B
import qualified Data.Heap as Heap
import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.Conc (numCapabilities)
import           Statistics.Sample
import           System.Random.MWC


newtype TmiCor = TmiCor (Int,Int,Double,Double)
type HashTable s k v = B.HashTable s k v
-- | Result: GeneIdx1, GeneIdx2, TMIcor-value, p-value, FDR
type Result = UV.Vector (Int,Int,Double,Double,Double)

data GData = GD
  { pVariable :: {-# UNPACK #-} !Int -- p >> n
  , nSample :: {-# UNPACK #-} !Int
  , _data :: GV.Vector v Double => v Double
  }

newtype TCor = TCor
  { unTCor :: ((Int,Int),Double) } deriving (Eq)

instance Ord TCor where
  -- comparing the abs value
  compare = compare `on` (abs.snd.unTCor)


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
    let n = GV.length label
        p = V.length vVec
    in
     go 0 Heap.empty $
     map
     (\ p@(i, j) ->
       let v = tCorStded (V.unsafeIndex vVec i) (V.unsafeIndex vVec j)
               label
       in TCor (p, v))
     [(i, j) | i <- [0 .. p - 1], j <- [i + 1 .. p - 1]]
  where go _ h [] = map unTCor $ toList h
        go acc h (x : xs)
          | acc < k = go (acc + 1) (Heap.insert x h) xs
          | otherwise = go (acc + 1) (Heap.insert x $ Heap.deleteMin h) xs
    
tmiCor :: (GV.Vector v Bool)
       => Seed -- ^ initial random seed
       -> Int  -- ^ repeat times for permutation
       -> Int  -- ^ first k gene pairs
       -> GData -- ^ vector of gene chip
       -> v Bool
       -> (Result,Seed)
{-# INLINABLE tmiCor #-}       
tmiCor seed nPerm kPairs (GD p n _data) label' =
  let toSlices :: GV.Vector v Double => v Double -> [v Double]
      toSlices v = map ((\beg -> GV.slice beg n v).(*n)) [0..p-1]
      {-# INLINE toSlices #-}
      n1 = GV.foldl' (\acc e -> if e then acc + 1 else acc) 0 label'
      label = GV.replicate n1 True UV.++ 
              GV.replicate (GV.length label' - n1) False
      vec = UV.concat $
            map
            (\v ->
              let (v1,v2) = ipartition' v label'
                  (m1,var1) = meanVarianceUnb v1 
                  (m2,var2) = meanVarianceUnb v2
              in (GV.++)
                 (GV.map (\e -> (e - m1) / sqrt var1) v1)
                 (GV.map (\e -> (e - m2) / sqrt var2) v2)                 
            ) $ toSlices _data
      vVec = V.fromList $ toSlices vec
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
     forM_ [(i,j) | i <- [0..p-1], j <- [i+1..p-1]] $ \idx@(i,j) ->
       let vs = sort $
                withStrategy
                (parListChunk (ceiling $
                               fromIntegral nPerm /
                               fromIntegral numCapabilities) rdeepseq) $
                map
                (abs . tCor (V.unsafeIndex vVec i)
                 (V.unsafeIndex vVec j)) ls
           vs' = dropWhile (<= minCor) vs
       in do
         ex <- H.lookup hTable idx
         when (isJust ex) $
           H.insert pTable idx $ -- p-value
           fromIntegral (length $
                         dropWhile (<= abs (fromJust ex)) vs) /
           fromIntegral nPerm
         collect umv tCorVec vs'
     uv <- UV.unsafeFreeze umv
     let fdrVec = UV.map (\e -> fromIntegral e / fromIntegral (kPairs*nPerm)) $
                  UV.postscanr' (+) 0 uv      
     result <- liftM UV.reverse $ UV.generateM kPairs $ \k -> 
       let idx@(i,j) = UV.unsafeIndex gpVec k
           cor = UV.unsafeIndex tCorVec k
           fdr = UV.unsafeIndex fdrVec k
       in do
        p <- liftM fromJust $ H.lookup pTable idx 
        return (i,j,cor,p,fdr)
     return (result,seed')

     
collect :: (PrimMonad m, GMV.MVector v Int) => v (PrimState m) Int -> UV.Vector Double -> [Double] -> m ()
{-# INLINABLE collect #-}
collect gmv tCorVec = go 1
  where
    go idx vs | idx == UV.length tCorVec = do
      n <- GMV.unsafeRead gmv (idx-1)
      GMV.unsafeWrite gmv (idx-1) (n + length vs)
              | otherwise =
      let (vs1,vs2) = break (<= abs (UV.unsafeIndex tCorVec idx)) vs
      in do
        n <- GMV.unsafeRead gmv (idx-1)
        GMV.unsafeWrite gmv (idx-1) (n + length vs1)
        go (idx+1) vs2

        
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
        
pcc :: (GV.Vector v1 Double
      ,GV.Vector v2 Double
      ,v1 ~ v2)
    => v1 Double
    -> v2 Double
    -> Double
{-# INLINABLE pcc #-}    
pcc v1 v2 | GV.length v1 == GV.length v2 =
  let m1 = mean v1
      m2 = mean v2
  in (\(num,res1,res2) ->
       num / sqrt (res1*res2)) $
     GV.ifoldl'
     (\(num,res1,res2) i e1 ->
       let e2 = GV.unsafeIndex v2 i
           r1 = e1-m1
           r2 = e2-m2
           num' = num+r1*r2
           res1' = res1+r1*r1
           res2' = res2+r2*r2
       in (num',res1',res2')
     ) (0,0,0) v1
          | otherwise = error "pcc: input vec did not have equal length."


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
            
ipartition' :: (GV.Vector v1 a
              ,GV.Vector v2 Bool) --,v1 ~ v2)
            => v1 a -> v2 Bool
            -> (v1 a, v1 a)
{-# INLINABLE ipartition' #-}            
ipartition' vec boolVec | GV.length vec == GV.length boolVec =
  let n = GV.length vec
  in runST $ do
    mv <- GMV.unsafeNew n
    (_,l,_) <- GV.foldM'
               (\(idx,i,j) e ->
                 let !idx' = idx + 1
                 in if e
                    then let !i' = i + 1
                         in GMV.unsafeWrite mv i (GV.unsafeIndex vec idx) >>
                            return (idx',i',j)
                    else let !j' = j - 1
                         in GMV.unsafeWrite mv j (GV.unsafeIndex vec idx) >>
                            return (idx',i,j')
               ) (0,0,n-1) boolVec
    v1 <- GV.unsafeFreeze $ GMV.slice 0 l mv
    v2 <- GV.unsafeFreeze $ GMV.slice l (n-l) mv
    return (v1,v2)
                        | otherwise = error "ipartition': input vec did not have equal length."
                                      
ipartition :: (GV.Vector v1 a
             ,GV.Vector v2 Bool) --v1 ~ v2)
           => v1 a -> v2 Bool
           -> (v1 a, v1 a)
{-# INLINABLE ipartition #-}           
ipartition vec boolVec =
  let (v1,v2) = ipartition' vec boolVec
  in runST $ do
    mv2 <- GV.unsafeThaw v2
    GMV.reverse mv2
    v2' <- GV.unsafeFreeze mv2
    return (v1,v2')

permute :: GV.Vector v a => Seed -> Int -> v a -> ([v a],Seed)
{-# INLINABLE permute #-}
permute s n vec =
  let l = GV.length vec
      v = GV.concat $ replicate n vec
      t = n * l
      end = l - 1
  in runST $ do
    mv <- GV.unsafeThaw v
    gen <- restore s
            
    forM_ [0..t-1] $ \idx -> do
      let (c,r) = idx `divMod` l
          i = c * l
      i' <- uniformR (r,end) gen
      GMV.unsafeSwap mv idx (i+i') 

    v' <- GV.unsafeFreeze mv
    s' <- save gen
    return (map ((\i -> GV.unsafeSlice i l v').(*l)) [0..n-1] ,s')

