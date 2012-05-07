-----------------------------------------------------------------------------
-- | Functions for internal usage.
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------

module GA.Internal where

import Data.Ord
import Data.List
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V


atUV :: UV.Unbox a => UV.Vector a -> Int -> a
{-# INLINE atUV #-}
atUV = UV.unsafeIndex

atV :: V.Vector a -> Int -> a
{-# INLINE atV #-}
atV = V.unsafeIndex

splitEvery :: Int -> [a] -> [[a]]
{-# INLINE splitEvery #-}
splitEvery _ [] = []
splitEvery n xs = let (as,bs) = splitAt n xs 
                  in as : splitEvery n bs 
