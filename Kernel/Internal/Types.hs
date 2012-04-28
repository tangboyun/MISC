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


import Data.Packed.Matrix


type FloatType = Double
-- | Kernel Matrix
newtype KMatrix = K (Matrix FloatType)
-- | Hessian Matrix
newtype HMatrix = H (Matrix FloatType)
