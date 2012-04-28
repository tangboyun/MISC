{-# LANGUAGE Rank2Types #-}
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
module Main where

import PValueAdj.RVM
import Statistics.Basic
import Statistics.Sampling
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM
import Data.Packed
import System.Random.MWC
import System.Random.MWC.Distributions
import Control.Monad.ST
import Control.Monad
import System.Environment

main :: IO ()
main = do
  nSample:nVar:nRepeat:_ <- fmap (map (read :: String -> Int)) getArgs
  g' <- create
  s <- save g'
  -- alpha = 3, beta = 1 
  let k = 3.0
      b = 1.0
      f = gamma k (1/b)
  countV <- 
    foldM (\((accRVM,accP),seed) _ ->
            
            undefined
            ) ((UV.replicate nVar (0 :: Int)
              ,UV.replicate nVar (0::Int)),s) [0..nRepeat-1]
  undefined

tTest :: ExpData -> ExpData -> TestType -> Vector FloatType
tTest group1 group2 tp = getPfromT (fromList $ UV.toList t) df tp
  where
    n1 = fromIntegral $ V.length group1
    n2 = fromIntegral $ V.length group2
    df = n1 + n2 - 2
    g1 = V.map (UV.fromList . toList) group1
    g2 = V.map (UV.fromList . toList) group2
    (m1,v1) = vecFastMeanVar g1
    (m2,v2) = vecFastMeanVar g2
    t = UV.zipWith (/) (UV.zipWith (-) m1 m2)
        (UV.map (sqrt . (* (1/n1 + 1/n2)) . (/ df)) $ 
         UV.zipWith (+) 
         (UV.map (* (n1 - 1)) v1)
         (UV.map (* (n2 - 1)) v2))

