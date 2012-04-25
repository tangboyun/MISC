{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
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

module PAdj
       (
         bhFDR
       , bonferroni
       , rankAll
       )

       where

import qualified Data.Vector.Unboxed as UV
import Data.List
import Data.Function

type FloatType = Double
type PValues = UV.Vector FloatType

{-# INLINE bhFDR #-}
bhFDR :: PValues -> PValues
bhFDR pVec = 
  let nSample = fromIntegral $ UV.length pVec
  in UV.zipWith (/) (UV.map (* nSample) pVec) $ rankAll pVec
  
{-# INLINE bonferroni #-}
bonferroni :: PValues -> PValues
bonferroni pVec = UV.map (* (fromIntegral $ UV.length pVec)) pVec
  
{-# INLINE rankAll #-}
-- | 等价于R中的rank
rankAll :: UV.Vector FloatType -> UV.Vector FloatType  
rankAll vec =
  let n = UV.length vec
  in UV.fromList $ map fst $
     sortBy (compare `on` snd) $ concat $ 
     map (\ls@((idx,v):_) -> 
           let (acc_sum,acc_len) = foldl' (\(acc_s,acc_l) e -> 
                                             let !acc_s' = acc_s + e
                                                 !acc_l' = acc_l + 1
                                             in (acc_s',acc_l')
                                           ) (0,0) $ map fst ls
               meanIdx = acc_sum / acc_len
           in map (\(_,v) -> (meanIdx,v)) ls) $ 
     groupBy (\(_,v1) (_,v2) -> vec `at` v1 == vec `at` v2) $ zip [1..] $
     sortBy (compare `on` (vec `at`)) [0..n-1]
  where 
    at = UV.unsafeIndex
