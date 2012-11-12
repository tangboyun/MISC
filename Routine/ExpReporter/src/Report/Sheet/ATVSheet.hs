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

module Report.Sheet.ATVSheet where

import           Control.Arrow ((&&&))
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Colour.Names
import           Data.List hiding (isSuffixOf)
import           Data.Maybe
import qualified Data.Vector as V
import           Report.Sheet.Styles
import           Report.Sheet.Template
import           Text.StringTemplate
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Util
import           Report.Types
import           Report.Sheet.UtilFun
import           Debug.Trace


allTargetWB :: Setting -> ByteString -> (Workbook, [ByteString])
allTargetWB set@(Setting c r _ _) str =
  let ss = B8.lines str
      (hs,rs) = span ((== '#') . B8.head) ss
      atHeadStr = parseATheads $ map B8.unpack hs
      (hraw:rss) = map (V.fromList . B8.split '\t') rs
      ts = removeUnusedAnno set $ reorganize $
           V.map removeDQ hraw : rss 
      h = head ts 
      j = fromJust $ V.elemIndex "Number Passed" h
      at = V.unsafeIndex
      ts' = 
        case V.elemIndex "ControlType" h of
          Nothing -> map (V.ifilter (\idx _ -> idx /= j)) (h:tail ts)
          Just i -> map (V.ifilter (\idx _ -> idx /= i && idx /= j)) $
                    h : filter (\e -> e `at` i == "false") (tail ts)
      header = head ts'
      f suffix vec = V.minimum &&& V.maximum $ V.findIndices
                        (suffix `isSuffixOf`) vec
      (rawBeg,rawEnd) = f "(raw)" header
      (norBeg,norEnd) = f "(normalized)" header
      (annBeg,annEnd) = (norEnd + 1, len - 1)
      rows = map (\vec -> 
                    let cs = 
                          [string $ B8.unpack $ vec `at` 0] ++
                          (map (number . read . B8.unpack) $
                           V.toList $ V.slice rawBeg (norEnd-rawBeg+1) vec) ++
                          (map (toCell . B8.unpack) $
                           V.toList $ V.slice annBeg (annEnd-annBeg+1) vec)
                    in mkRow cs 
                  ) $ tail ts'
      attrs = let commonAttr = [("rawBeg",toStr rawBeg)
                               ,("rawEnd",toStr rawEnd)
                               ,("norBeg",toStr norBeg)
                               ,("norEnd",toStr norEnd)
                               ,("annBeg",toStr annBeg)
                               ,("annEnd",toStr annEnd)
                               ,("annos", B8.intercalate ", " $ V.toList $ V.drop annBeg header)
                               ,("atHeadStr", B8.pack atHeadStr)
                               ]
              in case r of
                Coding -> commonAttr
                NonCoding ->
                  let source = toStr $ fromJust $ V.elemIndex "source" header 
                      relaBeg = toStr $ fromJust $ V.elemIndex "relationship" header
                      relaEnd = toStr $ len -1
                  in commonAttr ++ 
                     [("source",source)
                     ,("relaBeg",relaBeg)
                     ,("relaEnd",relaEnd)]
      atNoteStr = render $
                  setManyAttrib attrs
                  $ allTargetTemplate set
      len = V.length header
      idxLen = fromIntegral $ length (filter (== '\n') $ allTargetStr set) + 2
      atNoteCell = string atNoteStr
                   # mergeAcross (fromIntegral $ len - 1)
                   # mergeDown idxLen
                   # withStyleID "allHead"
                   # addTextPropertyAtRanges [(0, fromJust $ elemIndex '\n' atNoteStr)]
                                             [Bold, Text $ dfp {size = Just 12}]
                   # addTextPropertyAtRanges (atNoteStr `match` log2Regex)
                                             [Bold, Text $ dfp {color= Just dodgerblue}]                    
      note = mkRow [atNoteCell]
      line1 = mkRow
              [emptyCell
              ,string "Raw Intensities"
               # mergeAcross (fromIntegral $ rawEnd-rawBeg)
               # withStyleID "riCell"
              ,string "Normalized Intensities"
               # mergeAcross (fromIntegral $ norEnd-norBeg)
               # withStyleID "niCell"
              ,string "Annotations"
               # mergeAcross (fromIntegral $ annEnd-annBeg)
               # withStyleID "annoCell"
              ]
      line2 = mkRow $
              map (withStyleID "boldCell" . string . B8.unpack) $ V.toList header
      (hs1,begIdx) = ([note
                     ,emptyRow # begAtIdx (idxLen + 2)
                     ,emptyRow # begAtIdx (idxLen + 3)
                     ,line1 # begAtIdx (idxLen + 4)
                     ,line2 # begAtIdx (idxLen + 5)]
                    ,idxLen + 6)
      table1 = mkTable $
               hs1 ++ zipWith (\idx row -> row # begAtIdx idx) [begIdx..] rows
      table strS = let idx = fromIntegral $ length (filter (== '\n') strS) + 4
                   in
                    mkTable
                    (mkRow
                     [string strS
                      # mergeAcross 15 --
                      # mergeDown idx 
                      # withStyleID "allHead"
                      # addTextPropertyAtRanges [(0, fromJust $ elemIndex '\n' strS)]
                                                [Bold, Text $ dfp {size = Just 12}]
                     ] : zipWith (\i row -> row # begAtIdx i)
                     [idx+2 ..] (replicate 100 emptyRow))
                    # withStyleID "white"
      tables =
        zip ["All Targets Value"
            ,"Box Plot"
            ,"Scatter Plot"
            ,"Clustering Map"] $
        table1 :
        map table
        [boxPlotStr
        ,scatterPlotStr
        ,render $ setAttribute "nSample" (norEnd-norBeg+1) $ clusteringTemplate set]
      addS wb = wb # addStyle (Name "boldCell") boldCell
                   # addStyle (Name "riCell") riCellStyle
                   # addStyle (Name "niCell") niCellStyle
                   # addStyle (Name "annoCell") annoCellStyle
                   # addStyle (Name "allHead") allHeadStyle
                   # addStyle (Name "Default") defaultS
                   # addStyle (Name "white") whiteCellStyle
      infos = hs ++ [B8.pack $ "Num of entities:" ++ " " ++ show ((fromIntegral $ B8.count '\n' str) - length hs - 1)]
  in trace (show ts) $
     case c of
    GE -> (addS $ mkWorkbook $ map (\(n,t) -> mkWorksheet (Name n) t) tables,infos)
    _  -> case r of
      Coding -> (addS $ mkWorkbook $
                 map (\(n,t) -> mkWorksheet (Name $ n ++ " - mRNAs") t) tables
               ,infos)
      NonCoding -> (addS $ mkWorkbook $
                    map (\(n,t) -> mkWorksheet (Name $ n ++ " - LncRNAs") t) tables
                  ,infos)


