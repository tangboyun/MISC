{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns #-}
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

module Trans where

import Diagrams.TwoD
import Graphics.Rendering.Diagrams

-- | 斜切变换
slant ang = fromLinear r (linv r)
  where
    r            = s1 theta <-> s2 theta
    Rad theta    = convertAngle ang
    s1 th (unr2 -> (x,y)) = r2 (x + cos th * y, sin th * y)
    s2 th (unr2 -> (x,y)) = r2 (x, cos th * x + sin th * y)



