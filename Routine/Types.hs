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

data CutOff = C
              !Double -- fold change
              !(Maybe Double) -- p
              deriving (Eq)

data Setting = Setting Chip RNA Species
               deriving (Eq)

data RNA = Coding
         | NonCoding
         deriving (Eq)

data Chip = GE
          | Lnc
          deriving (Eq)

data Species = Human
             | Rat
             | Mouse
             deriving (Eq)
