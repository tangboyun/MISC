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
module GA.Types 
       (
         Fitness
       , Individual(..)
       , Chrom(..)
       )
       
       where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV
import Data.Int
import Data.Word

type Fitness = Double
type Individual a = UV.Vector a
type Chrom a = V.Vector (Individual a)
                    
