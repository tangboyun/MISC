{-# LANGUAGE GADTs #-}
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

module Matrix where

import Data.Vector (Vector)

data MatrixOrder = RowMajor | ColumnMajor deriving (Show,Eq)

transOrder RowMajor = ColumnMajor
transOrder ColumnMajor = RowMajor

data Matrix a where
  Dense :: Num a =>
           { nRow :: {-# UNPACK #-} !Int
           , nCol :: {-# UNPACK #-} !Int
           , xdat :: {-# UNPACK #-} !(Vector a)
           , order :: !MatrixOrder
           } -> Matrix a
