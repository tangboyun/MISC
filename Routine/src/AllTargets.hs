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

module AllTargets where

import UtilFun
import Template
import Styles
import qualified Data.Vector as V
import Control.Arrow ((&&&))
import           Text.StringTemplate
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Util
import           Text.XML.SpreadsheetML.Writer (toElement)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Maybe
import Types
import Data.List hiding (isSuffixOf)

allTargetWB :: Setting -> ByteString -> (Workbook, [ByteString])
allTargetWB set@(Setting c r s) str =
  let ss = B8.lines str
      (hs,rs) = span ((== '#') . B8.head) ss
      atHeadStr = parseATheads $ map B8.unpack hs
      ts = map (V.fromList . B8.split '\t') rs
      h = head ts
      j = fromJust $ V.elemIndex "Number Passed" h
      at = V.unsafeIndex
      ts' =
        case V.elemIndex "ControlType" h of
          Nothing -> map (V.ifilter (\idx _ -> idx /= j)) ts
          Just i -> map (V.ifilter (\idx _ -> idx /= i && idx /= j)) $
                    header : filter (\e -> e `at` i == "false") (tail ts)
      header = head ts'
      f suffix vec = V.minimum &&& V.maximum $ V.findIndices
                        (suffix `isSuffixOf`) vec
      (rawBeg,rawEnd) = f "(raw)" header
      (norBeg,norEnd) = f "(normalized)" header
      (annBeg,annEnd) = (norEnd + 1, len - 1)
      hrow = map (string . B8.unpack) $ V.toList header
      rows = map (\vec -> mkRow $ 
                    [string $ B8.unpack $ vec `at` 0] ++
                    (map (number . read . B8.unpack) $
                     V.toList $ V.slice rawBeg (norEnd-rawBeg) vec) ++
                    (map (toCell . B8.unpack) $
                     V.toList $ V.slice annBeg (annEnd-annBeg) vec)
                  ) $ tail ts'
      atNoteStr = B8.unpack $ render $
                  setManyAttrib
                  [("rawBeg",toStr rawBeg)
                  ,("rawEnd",toStr rawEnd)
                  ,("norBeg",toStr norBeg)
                  ,("norEnd",toStr norEnd)
                  ,("annBeg",toStr annBeg)
                  ,("annEnd",toStr annEnd)
                  ,("annos", B8.intercalate ", " $ V.toList $ V.drop norEnd header)
                  ,("atHeadStr", B8.pack atHeadStr)
                  ] $ allTargetTemplate set
      len = V.length header
      idxLen = fromIntegral $ length (filter (== '\n') $ allTargetStr set) + 2
      atNoteCell = string atNoteStr
                   # mergeAcross (fromIntegral $ len - 1)
                   # mergeDown idxLen
                   # withStyleID "allHead"
                   # addTextPropertyAtRanges [(0, fromJust $ elemIndex '\n' atNoteStr)] [Bold, Text $ dfp {size = Just 14}]
      note = mkRow [atNoteCell]
      line1 = mkRow
              [emptyCell
              ,string "Raw Intensities" # mergeAcross (fromIntegral $ rawEnd-rawBeg) # withStyleID "riCell"
              ,string "Normalized Intensities" # mergeAcross (fromIntegral $ norEnd-norBeg) # withStyleID "niCell"
              ,string "Annotations" # mergeAcross (fromIntegral $ annEnd-annBeg) # withStyleID "annoCell"
              ]
      line2 = mkRow $
              map (withStyleID "boldCell" . string . B8.unpack) $ V.toList header
      (hs1,begIdx) = ([note
                     ,emptyRow # begAtIdx (idxLen + 2)
                     ,emptyRow # begAtIdx (idxLen + 3)
                     ,line1 # begAtIdx (idxLen + 4)
                     ,line2 # begAtIdx (idxLen + 5)]
                    ,idxLen + 6)
      table1 = mkTable $ hs1 ++ zipWith (\idx row -> row # begAtIdx idx) [begIdx..] rows
      table str = mkTable
                  [mkRow
                   [string str
                    # mergeAcross 17 --
                    # mergeDown (fromIntegral $ length (filter (== '\n') str) + 2)
                    # withStyleID "allHead"
                    # addTextPropertyAtRanges [(0, fromJust $ elemIndex '\n' str)] [Bold, Text $ dfp {size = Just 14}]
                   ]]
      tables =
        zip ["All Targets Value"
            ,"Box Plot"
            ,"Scatter Plot"
            ,"Hierarchical Clustering Map"] $
        table1 :
        map table
        [boxPlotStr
        ,scatterPlotStr
        ,render $ setAttribute "nSample" (norEnd-norBeg+1) clustringTemplate]
      addS wb = wb # addStyle (Name "boldCell") boldCell
                   # addStyle (Name "riCell") riCellStyle
                   # addStyle (Name "niCell") niCellStyle
                   # addStyle (Name "annoCell") annoCellStyle
                   # addStyle (Name "allHead") allHeadStyle
                   # addStyle (Name "Default") defaultS
      infos = hs ++ [B8.pack $ "Num of entities:" ++ " " ++ show (length rs - 1)]
  in case c of
    GE -> (addS $ mkWorkbook $ map (\(n,t) -> mkWorksheet (Name n) t) tables,infos)
    _  -> case r of
      Coding -> (addS $ mkWorkbook $ map (\(n,t) -> mkWorksheet (Name $ n ++ " - mRNAs") t) tables
               ,infos)
      NonCoding -> (addS $ mkWorkbook $ map (\(n,t) -> mkWorksheet (Name $ n ++ " - LncRNAs") t) tables
                  ,infos)
