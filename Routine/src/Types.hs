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

module Types where

import           Data.ByteString.Lazy.Char8 (ByteString)

data CutOff = C
              !Double -- fold change
              !(Maybe (TTest,Double)) -- p
              deriving (Eq)

data Setting = Setting Chip RNA Species Sheet
               deriving (Eq)

data RNA = Coding
         | NonCoding
         deriving (Eq)
data TTest = Paired
           | Unpaired
           deriving (Eq)
data Chip = GE
          | Lnc
          deriving (Eq)

data Species = Human
             | Rat
             | Mouse
             deriving (Eq)
newtype GroupPairs = G [(ByteString,ByteString)]
newtype SamplePairs = S [(ByteString,ByteString)]

data Sheet = Formula
           | Numeric
           deriving (Eq)
