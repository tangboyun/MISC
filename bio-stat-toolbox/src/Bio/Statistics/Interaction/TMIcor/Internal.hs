{-# LANGUAGE BangPatterns,RankNTypes,FlexibleContexts,TypeFamilies,PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2013 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------

module Bio.Statistics.Interaction.TMIcor.Internal

       where
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Par.Combinator
import           Control.Monad.Par.Scheds.Direct
import           Control.Monad.ST.Strict
import           Data.Function
import           Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           System.Random.MWC
import Control.Parallel.Strategies


newtype TCor = TCor
  { unTCor :: ((Int,Int),Double) } deriving (Eq)

instance Ord TCor where
  -- comparing the abs value
  compare = compare `on` (abs.snd.unTCor)

instance NFData TCor where
  rnf (TCor a) = rnf a 

atV :: V.Vector a -> Int -> a
atG :: GV.Vector v a => v a -> Int -> a
atUV :: UV.Unbox a => UV.Vector a -> Int -> a
{-# INLINE atV #-}
{-# INLINE atG #-}
{-# INLINE atUV  #-}


-- atV = (V.!)
-- atG = (GV.!)
-- atUV = (UV.!)
atV = V.unsafeIndex
atG = GV.unsafeIndex
atUV = UV.unsafeIndex


kLargest :: GV.Vector v Double
           => Int
           -> Int
           -> V.Vector (v Double)
           -> [((Int,Int),Double)]
{-# INLINE kLargest #-}
kLargest nTrue k vVec =
  let p = V.length vVec
      (x,y) = if odd p
              then (p,(p-1) `quot` 2)
              else (p-1,p `quot` 2)
      psVec = getVec p
  in map unTCor $ runPar $
     parMapReduceRangeThresh 1 (InclusiveRange 0 (x - 1))
     (\i ->
       let is = take y [i*y..]
           initIdx = head is
           initI = fst $ calcIdx psVec initIdx
       in return $ sort $
          map
          (\idx@(a,b) ->
            let v = tCorStded nTrue (vVec `atV` a) (vVec `atV` b)
            in TCor (idx,v)
          ) $ goIdx initI p $ is)
     (\as bs ->
       let (cs,l) = merge as bs
       in if l > k
          then return $ reverse $ take k cs
          else return $ reverse cs)
     []
  where
    {-# INLINE merge #-}
    merge :: Ord a => [a] -> [a] -> ([a],Int)
    merge = go 0 []
      where 
        go c accs [] bs = (reverse bs ++ accs, c+length bs)
        go c accs as [] = (reverse as ++ accs, c+length as)
        go c accs aas@(a:as) bbs@(b:bs) =
          if a <= b
          then go (c+1) (a:accs) as bbs
          else go (c+1) (b:accs) aas bs
           


count :: (GV.Vector v a, Eq a) => a -> v a -> Int
{-# INLINE count #-}
count e vec = go 0 0
  where
    n = GV.length vec
    go acc idx | idx < n, e == vec `atG` idx = go (acc+1) (idx+1)
               | idx < n = go acc (idx+1)
               | otherwise = acc

{-# INLINE calcIdx #-}
calcIdx :: UV.Vector Int -> Int -> (Int,Int)
calcIdx vec !idx =
  let i = getI
      j = getJ i
  in (i,j)
  where
    n = UV.length vec
    getJ i = (2*idx+i*i+3*i-2*i*n+2) `quot` 2
    getI = go 0
      where
        go acc | acc == n - 1 = acc
               | otherwise = if vec `atUV` (acc+1) > idx
                             then acc
                             else go (acc+1)
                             

goIdx :: Int -> Int -> [Int] -> [(Int,Int)]
{-# INLINE goIdx #-}
goIdx _ _ [] = []
goIdx i n (idx:idxs) =
  let j = getJ i idx
  in if j > n - 1
     then (i+1,getJ (i+1) idx) : goIdx (i+1) n idxs
     else (i,j) : goIdx i n idxs
  where
    getJ i' p = (2*p+i'*i'+3*i'-2*i'*n+2) `quot` 2


{-# INLINE getVec #-}
getVec :: Int -> UV.Vector Int
getVec n = UV.generate n (\i -> (2*i*n-i*i-i) `quot` 2)  


toStdScores :: (GV.Vector v Double, NFData (v Double)) => Int -> [v Double] -> [v Double]
{-# INLINE toStdScores #-}
toStdScores nTrue =
  withStrategy (evalList rdeepseq) . -- 移除该行会导致并行时出现计算错误
  map
  (\v -> 
    let n = GV.length v
        nFalse = n - nTrue
        v1 = GV.unsafeSlice 0 nTrue v
        v2 = GV.unsafeSlice nTrue nFalse v 
        (m1,var1) = meanVar v1 
        (m2,var2) = meanVar v2
    in runST $ do
      mv <- GV.unsafeThaw v
      forM_ [0..n-1] $ \idx -> do
        e <- GMV.unsafeRead mv idx
        if idx < nTrue
          then GMV.unsafeWrite mv idx ((e-m1) / sqrt var1)
          else GMV.unsafeWrite mv idx ((e-m2) / sqrt var2)
      GV.unsafeFreeze mv)


countBy :: UV.Vector Double -> [Double] -> [Int]
countBy v = go 0
  where 
    v' = UV.tail v
    n = UV.length v'
    go idx ds | idx < n =
      let (ds1,ds2) = break (> t) ds
          t = abs (v' `atUV` idx)
      in length ds1 : go (idx+1) ds2
              | otherwise = [length ds]
{-# INLINE countBy #-}


tCorStded :: (GV.Vector v1 Double
            ,GV.Vector v2 Double
            ,v1 ~ v2)
          => Int -> v1 Double -> v2 Double -> Double
{-# INLINE tCorStded #-}             
tCorStded nTrue vi vj =
  let (!v1i,!v2i) = GV.splitAt nTrue vi
      (!v1j,!v2j) = GV.splitAt nTrue vj
  in atanh (pccStded v1i v1j) -
     atanh (pccStded v2i v2j)

pccStded :: (GV.Vector v1 Double
           ,GV.Vector v2 Double
           ,v1 ~ v2)
         => v1 Double
         -> v2 Double
         -> Double
{-# INLINE pccStded #-}         
pccStded v1 v2 | GV.length v1 == GV.length v2 = 
  (/ fromIntegral (GV.length v1 - 1)) $
  GV.foldl1' (+) $ GV.zipWith (*) v1 v2
               | otherwise = error "pccStded: input vec did not have equal length."
            


toSlices :: GV.Vector v Double => Int -> v Double -> [v Double]
toSlices n v =
  let (p,r) = GV.length v `divMod` n
  in if r /= 0
     then error "toSlices: length v / n ~= 0"
     else map ((\beg -> GV.slice beg n v).(*n)) [0..p-1]
{-# INLINE toSlices #-}



permute :: GV.Vector v a => Seed -> Int -> v a -> ([v a],Seed)
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
      GMV.unsafeSwap mv idx $! (i+i') 

    v' <- GV.unsafeFreeze mv
    s' <- save gen
    let ls = map (\i ->
                   GV.unsafeSlice (i*l) l v'
                  ) [0..n-1]
    return (ls,s')
{-# INLINE permute #-}


data P3 = P3
          {-# UNPACK #-} !Int
          {-# UNPACK #-} !Double
          {-# UNPACK #-} !Double

mres :: GV.Vector v Double => v Double -> P3
mres = GV.foldl' go (P3 0 0 0)
  where
    go (P3 n m s) x = P3 n' m' s'
      where n' = n + 1
            m' = m + d / fromIntegral n'
            s' = s + d * (x - m')
            d  = x - m

meanVar :: (GV.Vector v Double) => v Double -> (Double,Double)
meanVar = fini . mres
  where fini (P3 n m s)
          | n > 1     = (m,s / fromIntegral (n-1))
          | otherwise = (m,0)
{-# INLINE meanVar #-}

recomb :: (GV.Vector v1 Double,GV.Vector v2 Bool)
       => Int
       -> v2 Bool
       -> v1 Double -> UV.Vector Double
{-# INLINE recomb #-}
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
              let origE = dat `atG` (i * rowLen + tAcc + fAcc)
              in if e
                 then UMV.unsafeWrite mv (tBeg+tAcc) origE >>
                      return (tAcc+1,fAcc)
                 else UMV.unsafeWrite mv (fBeg+fAcc) origE >>
                      return (tAcc,fAcc+1)
              ) (0,0) label
       UV.unsafeFreeze mv
