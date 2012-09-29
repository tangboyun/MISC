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

module Label where

import Data.Function
import Data.List

data Cluster a = S Int a
               | L Int [a]
               | C Int [Cluster a]



sumC :: Cluster a -> Int
sumC (S _ _) = 1
sumC (L _ ls) = length ls
sumC (C _ cs) = foldr1 (+) $ map sumC cs

















