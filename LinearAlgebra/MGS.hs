{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}
{-# OPTIONS_GHC -fno-spec-constr-count #-}
-----------------------------------------------------------------------------
-- |
-- Module : Modified Gram-Schmidt
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------
module MGS where

import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double 
import qualified Prelude

<.> :: [:Double:] -> [:Double:] -> Double
<.> xs ys = sumP [: x * y | x <- xs | y <- ys :]

norm :: [:Double:] -> Double
norm xs = sqrt $ xs <.> xs

normalize :: [:Double:] -> [:Double:]
normalize xs =
  let n = norm xs
  in [: x/n | x <- xs :]
  
mgs_impl :: [:[: Double :]:] -> [:[: Double :]:]
mgs_impl cols = undefined
