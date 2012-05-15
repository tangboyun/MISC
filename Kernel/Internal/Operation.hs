-----------------------------------------------------------------------------
-- |
-- Module : Operations using kernel tricks
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference : Kernel Methods for Pattern Analysis.
--             John Shawe-Taylor & Nello Cristianini - Cambridge University Press, 2004
-- TODO: 将可扩展的Kernel操作并入haskell-svm项目
--
-----------------------------------------------------------------------------
module Kernel.Internal.Operation
       
       where       

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util
import Kernel.Internal.Types  
import Data.Packed.Development

-- | K_hat = k(x,z) / sqrt (k(x,x)k(z,z))
normalize :: KMatrix -> KMatrix
normalize (K m) = 
  let d = diag $ 
          cmap ((1.0 /) . sqrt) $ 
          takeDiag m
  in K $ d <> m <> d

-- | Distance of two sample in their kernel space
disK :: KMatrix -> Int -> Int -> FloatType
disK (K m) i j = sqrt $ 
                 atM' m i i - 2 * atM' m i j +
                 atM' m j j
                 
-- | Expected distance from the centre of mass
expDisFromCentre :: KMatrix -> FloatType
expDisFromCentre (K m) = 
  let l = fromIntegral $ rows m
      d = takeDiag m
  in sqrt $
     sumElements d / l -
     sumElements m / (l * l)

rowMean :: Matrix FloatType -> Matrix FloatType
rowMean m = 
  let n = rows m
      p = cols m    
  in cmap (/ fromIntegral p) $ m <> ones n 1

colMean :: Matrix FloatType -> Matrix FloatType
colMean m =
  let n = rows m
      p = cols m    
  in cmap (/ fromIntegral n) $ ones 1 p <> m

-- | Centering points in kernel space
centering :: KMatrix -> KMatrix
centering (K m) = 
  let l = rows m
      l' = fromIntegral l
      d = colMean m
      e = sumElements d / l'
      j = ones l 1 <> d
  in K $ m `sub` j `sub` 
         trans j `add` 
         scale (e) (ones l l)

