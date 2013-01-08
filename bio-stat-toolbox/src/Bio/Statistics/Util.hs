{-# LANGUAGE BangPatterns,RankNTypes,FlexibleContexts,TypeFamilies, NoMonomorphismRestriction #-}
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
module Bio.Statistics.Util where

import           Control.Monad
import           Control.Monad.ST.Strict
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV

import           Statistics.Sample
import           System.Random.MWC


-- | Split the vector in two parts, the first one containing those elements that
-- are true in the bool vec and the second one those false. The order of the elements is not preserved
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

-- | The order of the elements is preserved
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
{-# INLINABLE permute #-}



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
