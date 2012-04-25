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
module RVM where

import Statistics.Distribution.Gamma
import qualified Data.Vector.Unboxed as UV
import Data.Packed.Matrix

type FloatType = Double 

designMatrix :: [Int] -> Matrix FloatType
designMatrix ns = 
  let l = length ns
  in fromLists $ 
     concatMap (\(row,n) -> 
           replicate n (replicate row 0 ++ 
                        [1.0] ++ 
                        replicate (l - row - 1) 0)
          ) $ zip [0..] ns
