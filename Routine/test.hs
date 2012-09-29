{-# LANGUAGE OverloadedStrings #-}
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

module Main where

import DiffExp
import           Text.XML.SpreadsheetML.Writer (showSpreadsheet)
import           Text.XML.SpreadsheetML.Builder
import Text.XML.SpreadsheetML.Types
import qualified Data.ByteString.Lazy.Char8 as B8
spair = ("1C","1B")

main :: IO ()
main = do
  str <- B8.readFile "mRNAall.txt"
  let ((s1,_),(s2,_)) = sampleSheet (C 2 Nothing) (parseTSV str) spair
  writeFile "test.xls" $ showSpreadsheet $ mkWorkbook [s1,s2] # addStyle (Name "upTitle") upTitle
                                                              # addStyle (Name "dnTitle") dnTitle
                                                              # addStyle (Name "boldCell") boldCell
                                                              # addStyle (Name "frCell") frCellStyle
                                                              # addStyle (Name "riCell") riCellStyle
                                                              # addStyle (Name "niCell") niCellStyle
                                                              # addStyle (Name "annoCell") annoCellStyle
                                                              # addStyle (Name "noteCell") noteCellStyle
                                                              # addStyle (Name "Default") defaultS
