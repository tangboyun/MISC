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
module CoxPH where

import Numeric.LinearAlgebra.Util
import Numeric.Container

type FloatType = Double

centering :: Matrix FloatType -> Matrix FloatType
centering m = 
  let n = rows m -- sample
  in m `sub` ones n 1 <> (scale (1 / fromIntegral n) $ ones 1 n <> m)
      
coxReg :: Matrix FloatType -> Vector FloatType -> Vector Bool -> Vector (FloatType,FloatType)
coxReg mX' vS vF =
  let mX = centering mX'
  in 
  undefined
