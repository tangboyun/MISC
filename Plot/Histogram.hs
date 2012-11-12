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

histogram :: (GV.Vector v Double, Renderable (Path R2) b) => Int -> Colour Double -> v Double -> Diagram b R2
histogram nBin c vec =
  let (low,up) = H.range nBin vec
      w = (up - low) / fromIntegral nBin
      v = H.histogram_ nBin low up vec ::  UV.Vector Double
      factorX = UV.maximum v / (0.618 * (up - low))
  in scaleX factorX . alignBL . hcat . map (safeRect w c) . GV.toList $ v 
  where 
    safeRect w c h = if h /= 0
                     then rect w h
                          # alignB
                          # fillColor c
                     else hrule w
                          # alignB

main = do
  gen <- create
  vs <- replicateM 10000 (R.standard gen)
  defaultMain $ 
    histogram 75 red $ UV.fromList (vs :: [Double])
  
