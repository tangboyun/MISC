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
module Kernel.Internal.Operation
       
       where       

import Numeric.LinearAlgebra
import Kernel.Internal.Types  


normalize :: KernelMatrix -> KernelMatrix
normalize (K m) = 
  let d = diag $ 
          cmap ((1.0 /) . sqrt) $ 
          takeDiag m
  in K $ d <> m <> d
