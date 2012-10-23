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
              deriving (Eq,Show)

data FSet = F CutOff [(String,String)]
            deriving (Eq,Show)
                     
data Setting = Setting Chip RNA Species Sheet
               deriving (Eq)


data RNA = Coding
         | NonCoding
         deriving (Eq,Show)
data TTest = Paired
           | Unpaired
           deriving (Eq,Show)
data Chip = GE
          | Lnc
          deriving (Eq,Show)

data Species = Human
             | Rat
             | Mouse
             | Other
             deriving (Eq,Show)
newtype GroupPairs = G [(ByteString,ByteString)]
newtype SamplePairs = S [(ByteString,ByteString)]

data Sheet = Formula
           | Numeric
           deriving (Eq)
