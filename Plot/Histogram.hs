{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module : 一些柱状图相关便利函数
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------
module Main where

import Diagrams.Attributes
import Diagrams.TwoD
import qualified Statistics.Sample.Histogram as H
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import Statistics.Distribution
import Data.Colour
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.Colour.Names
import Function
import Statistics.Distribution.Normal
import Control.Monad
import System.Random.MWC
import qualified System.Random.MWC.Distributions as R
import Data.List
import Statistics.Sample
import Text.Printf

-- histogram :: (GV.Vector v Double, Renderable (Path R2) b) => Int -> Colour Double -> v Double -> Diagram b R2
histogram nBin c vec =
  let (low,up) = H.range nBin vec
      w = (up - low) / fromIntegral nBin
      v = H.histogram_ nBin low up vec ::  UV.Vector Double
      n = fromIntegral $ GV.length vec
      f = w * n / (cumulative d up - cumulative d low)
      factorX = UV.maximum v / (0.618 * (up - low))
      his = alignBL . hcat . map (safeRect w c) . GV.toList $ v
      ls = [low,low+w/2..up]
      d = normalFromSample vec
      vs = fromVertices $ map p2 $ zip ls $ map ((f *) . (density d)) ls
      funP = vs # stroke # lw 2 # lc green # moveOriginTo (p2 (low,f*(density d low)))
  in ((funP <> his) === strutY 10 ===
      xAxis (up - low) 2 (fromIntegral (ceiling low) - low) 2
     )
     # scaleX factorX
     # centerXY
     # pad 1.05
     
  where 
    safeRect w c h = if h /= 0
                     then rect w h
                          # alignB
                          # fillColor c
                     else hrule w
                          # alignB


xAxis len step beginAt w =
  let ls = takeWhile (<= len) [beginAt,beginAt + fromIntegral step..]
      ticks = map (\_ -> alignT $ vrule 5 # lw (w/2)) ls 
      vs = map p2 $ zip ls (repeat 0)
  in hrule len # lw w # alignL <>
     position (zip vs ticks)
-- xAxis :: Double -> Int -> Int -> Diagram b R2
-- xAxis len step beginAt =
--   let ls = takeWhile ((<= len).fromIntegral) [beginAt,beginAt+step..]
--   in (stroke (hrule len) # lw 0.1 # alignL # moveOriginTo (p2 (fromIntegral beginAt,0))) <>
--      hcat' (CatOpts Distrib (fromIntegral step) Proxy)
--      (map (\v ->
--             (vrule 0.3 # lw 0.05 # alignB)  ===
--             (text' (fromIntegral step / 4) (show v))) ls)
--   where
text' s t = text t # fontSize s <> strutY (1.2 * s)

units low up = last $
               takeWhile (\c -> (up - low) / c >= 4.5) $
               takeWhile (<= (up - low)) $
               concatMap (\c ->  map (10^^c *) [1,2,5])  [-4..]

fmtStr up low | up - low >= 5 = "%.0f"
              | up - low >= 0.5 = "%,1f"
              | up - low >= 0.25 = "%.2f"
              | up - low >= 0.125 = "%.3f"
              | otherwise = error "too small interval"

main = do
  gen <- create
  vs <- replicateM 10000 (R.standard gen)
  defaultMain $
    histogram 70 blue $ UV.fromList (vs :: [Double])
--    xAxis 10 2 0 # showOrigin
  
