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
module Kernel.Internal.Types where


import Numeric.LinearAlgebra


type FloatType = Double
newtype KernelMatrix = K (Matrix FloatType)
