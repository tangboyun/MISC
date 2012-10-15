{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module : I hate EXCEL!!!!
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------

module DiffExp where

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Char
import           Data.Colour.Names
import           Data.List (partition,findIndex,elemIndex)
import           Data.Maybe
import qualified Data.Vector as V
import           Styles
import           Template
import           Text.Printf
import           Text.StringTemplate
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Util
import           Text.XML.SpreadsheetML.Writer (toElement)
import           Types
import           UtilFun

mkDEGList :: CutOff -> Setting -> (V.Vector ByteString,[V.Vector ByteString]) -> [(ByteString,ByteString)] -> [(ByteString,[ByteString])]
mkDEGList c s x = foldr (\p@(s1,s2) acc ->
                        let ((_,gs1),(_,gs2)) = sampleSheet c s x p
                            str = s1 `B8.append` " vs " `B8.append` s2
                        in  (str `B8.append` "_up", gs1):
                            (str `B8.append` "_down", gs2):acc
                      ) []
  
mkFCWorkbook :: CutOff -> Setting -> (V.Vector ByteString,[V.Vector ByteString]) -> [(ByteString,ByteString)] -> Workbook
mkFCWorkbook c s x ps = 
  let ls = foldr (\e acc ->
                   let ((ws1,_),(ws2,_)) = sampleSheet c s x e
                   in ws1:ws2:acc
                 ) [] ps
  in mkWorkbook ls # addStyle (Name "upTitle") upTitle
                   # addStyle (Name "dnTitle") dnTitle
                   # addStyle (Name "boldCell") boldCell
                   # addStyle (Name "frCell") frCellStyle
                   # addStyle (Name "riCell") riCellStyle
                   # addStyle (Name "niCell") niCellStyle
                   # addStyle (Name "annoCell") annoCellStyle
                   # addStyle (Name "noteCell") noteCellStyle
                   # addStyle (Name "Default") defaultS

sampleSheet :: CutOff -> Setting -> (V.Vector ByteString,[V.Vector ByteString])
            -> (ByteString,ByteString) -> ((Worksheet,[ByteString]),(Worksheet,[ByteString]))
sampleSheet (C f _) setting@(Setting chip rna _) (header,vecs) (s1,s2) =
  let rawIdxs = V.findIndices ("(raw)" `isSuffixOf`) header
      norIdxs = V.findIndices ("(normalized)" `isSuffixOf`) header
      gsIdx = fromJust $ V.findIndex (== "GeneSymbol") header
      at = V.unsafeIndex
      rawIdxS1 = fromJust $ V.find ((== s1). extractS . (header `at`)) rawIdxs
      rawIdxS2 = fromJust $ V.find ((== s2). extractS . (header `at`)) rawIdxs
      norIdxS1 = fromJust $ V.find ((== s1). extractS . (header `at`)) norIdxs
      norIdxS2 = fromJust $ V.find ((== s2). extractS . (header `at`)) norIdxs
      
      fc i j vec = let v = (read $! B8.unpack $! vec `at` i) - (read $! B8.unpack $! vec `at` j)
                   in (signum v * (2 ** abs v),vec)
      mkRowIdx isUp vec = let (minI,maxI) = findNumPart header
                              ls = [0..V.length header - 1]
                              headIdx = V.fromList $ take minI ls
                              annoIdx = V.fromList $ drop (maxI + 1) ls
                              f1 = map (string . B8.unpack) . V.toList
                              f2 = map (number . read . B8.unpack) . V.toList
                              f3 = map (toCell . B8.unpack) . V.toList
                              reg = if isUp then "up" else "down"
                          in (mkRow $ 
                              f1 (V.unsafeBackpermute vec headIdx) ++
                              map formula [fcFormula,lfcFormula,afcFormula] ++ [string reg] ++
                              f2 (V.unsafeBackpermute vec $
                               V.fromList [rawIdxS1,rawIdxS2,norIdxS1
                                          ,norIdxS2]) ++
                              f3 (V.unsafeBackpermute vec annoIdx)
                             ,vec `at` gsIdx)
      tabHeader isUp = let (minI,maxI) = findNumPart header
                           ls = [0..V.length header - 1]
                           begPart = V.toList $ V.unsafeBackpermute header $
                                     V.fromList $ take minI ls
                           endPart = V.toList $ V.unsafeBackpermute header $
                                     V.fromList $ drop (maxI + 1) ls
                           fcStr = map (render . setManyAttrib [("s1",s1),("s2",s2)]) 
                                   [fcTemplate,lfcTemplate,afcTemplate,rgTemplate]
                           titleLs = begPart ++ fcStr ++
                                     map (header `at`) [rawIdxS1,rawIdxS2,norIdxS1,norIdxS2] ++
                                     endPart
                           len = length titleLs
                           regStr = if isUp then "up" else "down"
                           titleS = if isUp then "upTitle" else "dnTitle"
                           attrs  = let commonAttr = [("s1",s1)
                                                     ,("s2",s2)
                                                     ,("fc",B8.pack $ printf "%.1f" f) 
                                                     ,("annBeg", toStr $ length begPart + 8)
                                                     ,("annEnd", toStr $ len - 1)
                                                     ,("annos", foldl1 (\a b -> a `B8.append` ", " `B8.append` b) endPart)]
                                    in case rna of
                                      Coding    -> commonAttr
                                      NonCoding ->
                                        let source = toStr $ fromJust $ elemIndex "source" titleLs 
                                            relaBeg = toStr $ fromJust $ elemIndex "relationship" titleLs
                                            relaEnd = toStr $ len -1
                                        in commonAttr ++ 
                                           [("source",source)
                                           ,("relaBeg",relaBeg)
                                           ,("relaEnd",relaEnd)]
                           noteStr = B8.unpack $ render $
                                     setManyAttrib attrs $
                                     sampleTemplate setting
                           noteCell = string noteStr
                                      # mergeAcross (fromIntegral $ len - 1)
                                      # mergeDown idxLen
                                      # withStyleID "noteCell"
                                      # addTextPropertyAtRanges (noteStr `match` vsRegex) [Bold, Text $ dfp {color = Just red}]
                                      # addTextPropertyAtRanges (noteStr `match` cutOffRegex) [Bold, Text $ dfp {color = Just red}] 
                                      # addTextPropertyAtRanges (noteStr `match` log2Regex) [Bold, Text $ dfp {color= Just dodgerblue}] 
                           note = mkRow [noteCell ]
                           mol = case chip of
                                  GE -> "genes"
                                  _  -> case rna of
                                    Coding -> "mRNAs"
                                    _      -> "LncRNAs"
                           line1 = mkRow
                                   [withStyleID titleS $
                                    mergeAcross (fromIntegral $ len - 1) $
                                    string $ B8.unpack $ render $
                                    setManyAttrib [("fc", B8.pack $ printf "%.1f" f)
                                                  ,("s1",s1)
                                                  ,("s2",s2)
                                                  ,("reg",regStr)
                                                  ,("mol",mol)] tabHeaderTemplate
                                   ]
                           line2 = mkRow
                                   [emptyCell 
                                   ,string "Fold Change and Regulation" # mergeAcross 3 # withStyleID "frCell"
                                   ,string "Raw Intensities" # mergeAcross 1 # withStyleID "riCell"
                                   ,string "Normalized Intensities" # mergeAcross 1 # withStyleID "niCell"
                                   ,string "Annotations" # mergeAcross (fromIntegral $ length endPart - 1) # withStyleID "annoCell"
                                   ]
                           line3 = mkRow $
                                   map (withStyleID "boldCell" . string . B8.unpack) titleLs
                           idxLen = fromIntegral $ length (filter (== '\n') $ sampleStr setting) + 2  -- 首尾各空一行                                 
                       in 
                          ([note
                          ,emptyRow # begAtIdx (idxLen + 2)
                          ,emptyRow # begAtIdx (idxLen + 3)
                          ,line1 # begAtIdx (idxLen + 4)
                          ,line2 # begAtIdx (idxLen + 5)
                          ,line3 # begAtIdx (idxLen + 6)]
                          , idxLen + 7) 
      ((upRows,upGs),(dnRows,dnGs)) =
          uncurry
          (\a b ->
            (unzip $ map (mkRowIdx True . snd) a
            ,unzip $ map (mkRowIdx False . snd) b)) $
          partition ((> 0) . fst) $
          filter ((>= f) . abs . fst) $ map (fc norIdxS1 norIdxS2) vecs
      upSheet = let (hs,begIdx) = tabHeader True
                in mkWorksheet (Name $ B8.unpack s1 ++ " vs " ++ B8.unpack s2 ++ "_up") $
                   mkTable $ hs ++ zipWith (\idx row -> row # begAtIdx idx) [begIdx..] upRows
      dnSheet = let (hs,begIdx) = tabHeader False
                in mkWorksheet (Name $ B8.unpack s1 ++ " vs " ++ B8.unpack s2 ++ "_down") $
                   mkTable $ hs ++ zipWith (\idx row -> row # begAtIdx idx) [begIdx..] dnRows
  in ((upSheet,upGs),(dnSheet,dnGs))
  
groupSheet :: CutOff -> (V.Vector ByteString,[V.Vector ByteString]) -> (ByteString,ByteString) -> Worksheet
groupSheet (C f p) (header,vecs) (gs1,gs2) =
  let rawIdxs = V.findIndices ("(raw)" `isSuffixOf`) header
      norIdxs = V.findIndices ("(normalized)" `isSuffixOf`) header
      (rawIdxG1,rawIdxG2) = V.partition ((== gs1). extractG . (header `V.unsafeIndex`)) rawIdxs
      (norIdxG1,norIdxG2) = V.partition ((== gs1). extractG . (header `V.unsafeIndex`)) norIdxs
  in undefined
  
