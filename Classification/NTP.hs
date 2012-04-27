-----------------------------------------------------------------------------
-- |
-- Module : Nearest Template Prediction
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference : Hoshida Y (2010) Nearest Template Prediction: 
--             A Single-Sample-Based Flexible Class Prediction with Confidence Assessment. 
--             PLoS ONE 5(11): e15543. doi:10.1371/journal.pone.0015543
-- 
--
-----------------------------------------------------------------------------
module Classification.NTP 
       (
         nearestTemplatePrediction
       )
       where

import qualified Data.Vector.Unboxed as UV

type FloatType = Double
data P3 = P {-# UNPACK #-} !FloatType
            {-# UNPACK #-} !FloatType
            {-# UNPACK #-} !FloatType

{-# INLINE cosSim #-}
cosSim :: UV.Vector FloatType -> UV.Vector FloatType -> FloatType
cosSim v1 v2 = 
  let (P nu de1 de2) =
        UV.foldl' 
        (\(P n1 d1 d2) (x1,x2) -> 
          let n1' = n1 + x1*x2 
              d1' = d1 + x1*x1
              d2' = d2 + x2*x2
          in P n1' d1' d2'
        ) (P 0 0 0) $ UV.zip v1 v2
  in nu / (sqrt $ de1 * de2)

{-# INLINE cosDis #-}
cosDis :: UV.Vector FloatType -> UV.Vector FloatType -> FloatType
cosDis = ((1 -) .) . cosSim

{-# INLINE nearestTemplatePrediction #-}
nearestTemplatePrediction :: UV.Vector FloatType -> UV.Vector FloatType -> UV.Vector FloatType -> (Int,(FloatType,FloatType))
nearestTemplatePrediction template weight sample = 
  let sample' = UV.zipWith (*) sample weight
      posTemplate = template
      negTemplate = UV.map negate template
      dis1 = cosDis sample' posTemplate
      dis2 = cosDis sample' negTemplate
      label = if dis1 < dis2 
              then 1
              else -1
  in (label,(dis1,dis2))
