{-# LANGUAGE NoMonomorphismRestriction #-}
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
import Diagrams.Backend.Cairo.Internal

histogram' nBin (vec,c) (d,c') =
  let (low,up) = H.range nBin vec
      w = (up - low) / fromIntegral nBin
      v = H.histogram_ nBin low up vec ::  UV.Vector Double
      n = fromIntegral $ GV.length vec
      his = alignBL . hcat . map (safeRect w c) . GV.toList $ v
      ls = [low,low+w/2..up]
      maxH = UV.maximum v
      factorX = maxH / (0.618 * (up - low))
      fontS = maxH / 20
      vs = fromVertices $ map p2 $ zip ls $ map ((f *) . (density d)) ls
      f = w * n / (cumulative d up - cumulative d low)
      funP = vs # stroke # lw (fontS/10) # lcA c' # moveOriginTo (p2 (low,f*(density d low)))
  in (yAxis maxH fontS ||| strutX (fontS/2) |||
      ((funP <> his) # scaleX factorX === strutY (fontS/2) ===
       xAxis low up fontS factorX))
     # centerXY
     # pad 1.05
  where 
    safeRect w c h = if h /= 0
                     then rect w h
                          # alignB
                          # fcA c
                     else hrule w
                          # alignB


histogram nBin c vec =
  let (low,up) = H.range nBin vec
      w = (up - low) / fromIntegral nBin
      v = H.histogram_ nBin low up vec ::  UV.Vector Double
      n = fromIntegral $ GV.length vec
      his = alignBL . hcat . map (safeRect w c) . GV.toList $ v
      maxH = UV.maximum v
      factorX = maxH / (0.618 * (up - low))
      fontS = maxH / 20
  in (yAxis maxH fontS ||| strutX (fontS/2) |||
      (his # scaleX factorX === strutY (fontS/2) ===
       xAxis low up fontS factorX))
     # centerXY
     # pad 1.05
  where 
    safeRect w c h = if h /= 0
                     then rect w h
                          # alignB
                          # fcA c
                     else hrule w
                          # alignB

xAxis low up fSize factorX =
  let ls = takeWhile (<= len) [beginAt,beginAt + step..]
      beg = fromIntegral $ ceiling low
      ls' = take (length ls) [beg,beg+step..]
      len = up - low
      step = unit low up
      beginAt = beg - low 
      fmt = fmtStr beginAt (last ls) 
      ticks = map (\l ->
                    alignT $
                    (vrule (fSize/3) # lw (fSize/20) === strutY (fSize/2) ===
                     text' fSize (printf fmt l))
                  ) ls' 
      vs = map p2 $ zip (map (* factorX) ls) (repeat 0)
  in (hrule len # lw (fSize / 10) # alignL) # scaleX factorX <>
     position (zip vs ticks)

yAxis maxH fSize =
  let step = unit 0 maxH
      ls = takeWhile (<= maxH) [step,step+step..]
      fmt = fmtStr step (last ls)
      vs = map p2 $ zip (repeat 0) ls
      ticks = map (\l ->
                    alignR $
                    (text' fSize (printf fmt l) ||| strutX (fSize/2) |||
                     hrule (fSize/3) # lw (fSize/20))
                  ) ls
  in vrule maxH # lw (fSize / 10) # alignB <>
     position (zip vs ticks)
     
     
text' h t = text t # fontSize h <> rect (fromIntegral (length t) * 0.6 * h) h # lcA transparent

unit :: (Ord a,Floating a) => a -> a -> a
unit low up = last $
              takeWhile (\c -> (up - low) / c >= 4.5) $
              takeWhile (<= (up - low)) $
              concatMap (\c ->  map (10^^c *) [1,2,5])  [-4..]
{-# INLINABLE unit #-}

fmtStr low up | up - low >= 5 = "%.0f"
              | up - low >= 0.5 = "%.1f"
              | up - low >= 0.25 = "%.2f"
              | up - low >= 0.125 = "%.3f"
              | otherwise = error "too small interval"

main = do
  gen <- create
  randVec <- UV.replicateM 100000 (R.standard gen)
  defaultMain $ do
    let d = histogram' 100 (randVec,blue `withOpacity` 0.8) (normalFromSample randVec, green `withOpacity` 0.8) :: Diagram Cairo R2
        
        
    d 
