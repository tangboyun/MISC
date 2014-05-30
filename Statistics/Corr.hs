{-# LANGUAGE FlexibleContexts, BangPatterns #-}
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

module Rank where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM
import Data.Vector.Algorithms.Merge
import Control.Monad.ST
import Data.Function
import           Statistics.Distribution
import           Statistics.Distribution.StudentT

-- | Fractional ranking with ties. See http://en.wikipedia.org/wiki/Ranking
rank :: (UV.Unbox a,Ord a,UV.Unbox b,Enum b,Floating b) => UV.Vector a -> UV.Vector b
{-# INLINE rank #-}
rank v =
    let v2 = UV.indexed v
    in runST $ do
        m2 <- UV.unsafeThaw v2
        sortBy (compare `on` snd) m2
        v2' <- UV.unsafeFreeze m2
        let (idxV,sortedV) = UV.unzip v2'
        mr <- UVM.new (UV.length v)
        go sortedV mr 0
        r <- UV.unsafeFreeze mr
        mi <- UV.unsafeThaw (UV.indexed idxV)
        sortBy (compare `on` snd) mi
        idxV' <- fmap (fst . UV.unzip) $ UV.unsafeFreeze mi
        return (UV.unsafeBackpermute r idxV')
  where
    {-# INLINE go #-}
    go vec mv !i =
        if i >= UV.length vec
        then return ()
        else let n = countHead (UV.drop i vec)
                 r = sum (take n [fromIntegral i+1..]) / fromIntegral n
             in do
                 UVM.set (UVM.slice i n mv) r
                 go vec mv (i+n)
    {-# INLINE countHead #-}
    countHead vec =
        1 + checkHead 0 (UV.tail vec)
      where
        checkHead !i tV =
            if UV.unsafeIndex tV i == UV.unsafeIndex vec 0 &&
               i < UV.length vec
            then checkHead (i+1) tV
            else i

-- | Pearson product-moment correlation coefficient in one pass
corFast :: (UV.Unbox a,Floating a) => UV.Vector a -> UV.Vector a -> a
{-# INLINE corFast #-}
corFast vec1 vec2 =
    let 
        nd = fromIntegral n
        (x,y,x2,y2,xy) = go 0 0 0 0 0 0
        nu = nd * xy - x * y
        de = sqrt (nd * x2 - x * x) *
             sqrt (nd * y2 - y * y)
    in nu / de
  where
    n = UV.length vec1
    go !accX !accY !accX2 !accY2 !accXY !i | i < n =
        let x = UV.unsafeIndex vec1 i
            y = UV.unsafeIndex vec2 i
        in go (accX + x) (accY + y) (accX2 + x*x) (accY2+y*y) (accXY+x*y) (i+1)
                                           | otherwise = (accX,accY,accX2,accY2,accXY)

corTest :: (Real a,Floating a) => Int -> a -> a
{-# INLINE corTest #-}
corTest n r =
    let t = r * sqrt ((fromIntegral n - 2) / (1 - r * r))
        dis = studentT (fromIntegral n - 2)
    in realToFrac $ 2 * cumulative dis (realToFrac $ negate $ abs t)
