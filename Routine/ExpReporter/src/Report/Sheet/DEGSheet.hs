{-# LANGUAGE OverloadedStrings, FlexibleContexts, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module : 差异表达报表
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------

module Report.Sheet.DEGSheet where

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Colour.Names
import           Data.List (partition,elemIndex)
import           Data.Maybe
import qualified Data.Vector as V
import           Report.Sheet.Styles
import           Report.Sheet.Template
import           Report.Sheet.UtilFun
import           Report.Types
import           Statistics.Sample
import           Text.Printf
import           Text.StringTemplate
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Util

type Fun = CutOff -> Setting -> (V.Vector ByteString,[V.Vector ByteString])
            -> (ByteString,ByteString) -> ((Worksheet,[ByteString]),(Worksheet,[ByteString]))
type Result = ((Worksheet,[ByteString]),(Worksheet,[ByteString]))

volcanoPlotSheet :: CutOff -> Setting -> Worksheet
volcanoPlotSheet c s =
  let str = volcanoPoltStr c s
      idx = fromIntegral $ length (filter (== '\n') str) + 8
  in mkWorksheet (Name "Volcano Plots") $ mkTable
     (mkRow
      [string str
       # mergeAcross 17 --
       # mergeDown (idx - 2)
       # withStyleID "allHead"
       # addTextPropertyAtRanges [(0, fromJust $ elemIndex '\n' str)]
                                 [Bold, Text $ dfp {size = Just 14}]
      ] : zipWith (\i r -> r # begAtIdx i)
      [idx..] (replicate 100 emptyRow))
     # withStyleID "white"


     
addStyles :: Workbook -> Workbook
addStyles w = w # addStyle (Name "upTitle") upTitle
                # addStyle (Name "dnTitle") dnTitle
                # addStyle (Name "boldCell") boldCell
                # addStyle (Name "frCell") frCellStyle
                # addStyle (Name "riCell") riCellStyle
                # addStyle (Name "niCell") niCellStyle
                # addStyle (Name "annoCell") annoCellStyle
                # addStyle (Name "noteCell") noteCellStyle
                # addStyle (Name "title" ) title
                # addStyle (Name "groupRawCell") groupRawCellStyle
                # addStyle (Name "groupNorCell") groupNorCellStyle
                # addStyle (Name "allHead") allHeadStyle
                # addStyle (Name "white") whiteCellStyle
                # addStyle (Name "Default") defaultS
                

sampleSheet :: CutOff -> Setting -> (V.Vector ByteString,[V.Vector ByteString])
            -> (ByteString,ByteString) -> ((Worksheet,[ByteString]),(Worksheet,[ByteString]))
sampleSheet (C f _) setting@(Setting chip rna _ sheet) (header,vecs) (s1,s2) =
  let rawIdxs = V.findIndices ("(raw)" `isSuffixOf`) header
      norIdxs = V.findIndices ("(normalized)" `isSuffixOf`) header
      gsIdx = case rna of
                Coding -> fromJust $ V.elemIndex "GeneSymbol" header
                _      -> fromJust $ V.elemIndex "SeqID" header   
      at = V.unsafeIndex
      rawIdxS1 = fromJust $ V.find ((== s1). extractS . (header `at`)) rawIdxs
      rawIdxS2 = fromJust $ V.find ((== s2). extractS . (header `at`)) rawIdxs
      norIdxS1 = fromJust $ V.find ((== s1). extractS . (header `at`)) norIdxs
      norIdxS2 = fromJust $ V.find ((== s2). extractS . (header `at`)) norIdxs
      fc i j vec = let v = (read $ B8.unpack $ vec `at` i) - (read $ B8.unpack $ vec `at` j)
                   in (signum v * (2 ** abs v),vec)
      mkRowIdx isUp vec = let (minI,maxI) = findNumPart header
                              ls = [0..V.length header - 1]
                              headIdx = V.fromList $ take minI ls
                              annoIdx = V.fromList $ drop (maxI + 1) ls
                              n = V.length annoIdx
                              f1 = map (string . B8.unpack) . V.toList
                              f2 = map (number . read . B8.unpack) . V.toList
                              f3 = map (toCell . B8.unpack) . V.toList
                              reg = if isUp then "up" else "down"
                              fcV = fst $ fc norIdxS1 norIdxS2 vec
                              lfcV = signum fcV * (logBase 2 afcV)
                              afcV = abs fcV
                              calcPart = case sheet of
                                           Numeric -> map number [fcV,lfcV,afcV]
                                           _       -> map formula [fcFormula n,lfcFormula n,afcFormula n] -- rewrite
                          in (mkRow $ 
                              f1 (V.unsafeBackpermute vec headIdx) ++
                              calcPart ++ [string reg] ++
                              f3 (V.unsafeBackpermute vec annoIdx) ++
                              f2 (V.unsafeBackpermute vec $
                               V.fromList [rawIdxS1,rawIdxS2,norIdxS1
                                          ,norIdxS2])
                              
                             ,vec `at` gsIdx)
      tabHeader isUp = let (minI,maxI) = findNumPart header
                           ls = [0..V.length header - 1]
                           begPart = V.toList $ V.unsafeBackpermute header $
                                     V.fromList $ take minI ls
                           annPart = V.toList $ V.unsafeBackpermute header $
                                     V.fromList $ drop (maxI + 1) ls
                           nA = length annPart
                           fcStr = map (render . setManyAttrib [("s1",s1),("s2",s2)]) 
                                   [fcTemplate,lfcTemplate,afcTemplate,rgTemplate]
                           nB = length $ begPart ++ fcStr ++ annPart
                           titleLs = begPart ++ fcStr ++ annPart ++
                                     map (header `at`) [rawIdxS1,rawIdxS2,norIdxS1,norIdxS2]
                           len = length titleLs
                           regStr = if isUp then "up" else "down"
                           titleS = if isUp then "upTitle" else "dnTitle"
                           attrs  = let commonAttr = [("s1",s1)
                                                     ,("s2",s2)
                                                     ,("fc",B8.pack $ printf "%.1f" f)
                                                     ,("srBeg", toStr $ nB)
                                                     ,("srEnd", toStr $ nB+1)
                                                     ,("snBeg", toStr $ nB+2)
                                                     ,("snEnd", toStr $ nB+3)
                                                     ,("annBeg", toStr $ length begPart + 4)
                                                     ,("annEnd", toStr $ length begPart + 3 + nA)
                                                     ,("annos", foldl1 (\a b -> a `B8.append` ", " `B8.append` b) annPart)]
                                    in case rna of
                                      Coding    -> commonAttr
                                      NonCoding ->
                                        let source = toStr $ fromJust $ elemIndex "source" titleLs 
                                            relaBeg = toStr $ fromJust $ elemIndex "relationship" titleLs
                                            relaEnd = toStr $ (length $ begPart ++ fcStr ++ annPart) - 1
                                        in commonAttr ++ 
                                           [("source",source)
                                           ,("relaBeg",relaBeg)
                                           ,("relaEnd",relaEnd)]
                           noteStr = render $
                                     setManyAttrib attrs $
                                     sampleTemplate setting
                           noteCell = string noteStr
                                      # mergeAcross (fromIntegral $ len - 1)
                                      # mergeDown idxLen
                                      # withStyleID "noteCell"
                                      # addTextPropertyAtRanges (noteStr `match` vsRegex) [Bold, Text $ dfp {color = Just red}]
                                      # addTextPropertyAtRanges (noteStr `match` cutOffRegex) [Bold, Text $ dfp {color = Just red}] 
                                      # addTextPropertyAtRanges (noteStr `match` log2Regex) [Bold, Text $ dfp {color= Just dodgerblue}] 
                           note = mkRow [noteCell]
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
                                   ,string "Annotations" # mergeAcross (fromIntegral $ length annPart - 1) # withStyleID "annoCell"
                                   ,string "Raw Intensities" # mergeAcross 1 # withStyleID "riCell"
                                   ,string "Normalized Intensities" # mergeAcross 1 # withStyleID "niCell"
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
  
groupSheet :: CutOff -> Setting -> (V.Vector ByteString,[V.Vector ByteString])
           -> (ByteString,ByteString) -> ((Worksheet,[ByteString]),(Worksheet,[ByteString]))
groupSheet (C fcCutOff (Just (tCon,pCutOff))) setting@(Setting chip rna _ sheet) (header,vecs) (gs1,gs2) =
  let at = V.unsafeIndex
      gsIdx = case rna of
                Coding -> fromJust $ V.elemIndex "GeneSymbol" header
                _       -> fromJust $ V.elemIndex "SeqID" header   
      rawIdxs = V.findIndices (\e ->
                                let g = extractG e
                                in "(raw)" `isSuffixOf` e &&
                                   (g == gs1 || g == gs2)
                              ) header
      norIdxs = V.findIndices (\e ->
                                let g = extractG e
                                in "(normalized)" `isSuffixOf` e &&
                                   (g == gs1 || g == gs2)
                              ) header
      (rawIdxG1,rawIdxG2) = V.partition ((== gs1). extractG . (header `at`)) rawIdxs
      (norIdxG1,norIdxG2) = V.partition ((== gs1). extractG . (header `at`)) norIdxs
      n1 = V.length norIdxG1
      n2 = V.length norIdxG2
      extractNumeric vec = V.map (read . B8.unpack) . V.unsafeBackpermute vec 
      fc idxVec1 idxVec2 vec =
        let m1 = mean $ extractNumeric vec idxVec1
            m2 = mean $ extractNumeric vec idxVec2
            r = m1 - m2
        in signum r * (2 ** abs r)
      ttest idxVec1 idxVec2 vec =
        let tfun = case tCon of
              Paired -> ttestPaired
              _      -> ttestUnpaired
            v1 = extractNumeric vec idxVec1
            v2 = extractNumeric vec idxVec2
        in tfun v1 v2
      (ups,dns) = partition
                    (\v -> fc norIdxG1 norIdxG2 v > 0) $
                    filter
                    (\vec ->
                      let foldChange = fc norIdxG1 norIdxG2 vec
                          p = ttest norIdxG1 norIdxG2 vec
                      in abs foldChange >= fcCutOff &&
                         p <= pCutOff
                    ) vecs
      mkRowIdx isUp vec = let (minI,maxI) = findNumPart header
                              ls = [0..V.length header - 1]
                              headIdx = V.fromList $ take minI ls
                              annoIdx = V.fromList $ drop (maxI + 1) ls
                              annoLen = V.length annoIdx                 -- 2012.12.05
                              f1 = map (string . B8.unpack) . V.toList
                              f2 = map (number . read . B8.unpack) . V.toList
                              f3 = map (toCell . B8.unpack) . V.toList
                              reg = if isUp then "up" else "down"
                              ttestStr = render $
                                         setManyAttrib
                                         [("g1Beg",7+n1+n2+annoLen)
                                         ,("g1End",6+n1+n2+n1+annoLen)
                                         ,("g2Beg",7+n1+n2+n1+annoLen)
                                         ,("g2End",6+2*(n1+n2)+annoLen)
                                         ] $ toTTestTemplate tCon
                              fcAbsStr = render $
                                         setManyAttrib
                                         [("g1Beg",6+n1+n2+annoLen)
                                         ,("g1End",5+n1+n2+n1+annoLen)
                                         ,("g2Beg",6+n1+n2+n1+annoLen)
                                         ,("g2End",5+2*(n1+n2)+annoLen)
                                         ] gFCAbsTemplate
                              calcPart1 = case sheet of
                                            Numeric -> map number
                                                       [ttest norIdxG1 norIdxG2 vec
                                                       ,abs $ fc norIdxG1 norIdxG2 vec]
                                            _       -> map formula [ttestStr,fcAbsStr]
                              calcPart2 = case sheet of
                                            Numeric -> map number
                                                       [mean $ extractNumeric vec rawIdxG1
                                                       ,mean $ extractNumeric vec rawIdxG2
                                                       ,mean $ extractNumeric vec norIdxG1
                                                       ,mean $ extractNumeric vec norIdxG2                                                        
                                                       ]
                                            _       -> map formula [g1RawStr,g2RawStr,g1NorStr,g2NorStr]
                              g1RawStr = avgStr (4+annoLen) (3+n1+annoLen)
                              g2RawStr = avgStr (3+n1+annoLen) (2+n1+n2+annoLen)
                              g1NorStr = avgStr (2+n1+n2+annoLen) (1+n1+n2+n1+annoLen)                         
                              g2NorStr = avgStr (1+n1+n2+n1+annoLen) (2*(n1+n2)+annoLen)                       
                          in (mkRow $
                              f1 (V.unsafeBackpermute vec headIdx) ++
                              calcPart1 ++ [string reg] ++         -- 2012.12.05 make annos before intensities
                              f3 (V.unsafeBackpermute vec annoIdx) ++                              
                              calcPart2 ++
                              f2 (V.unsafeBackpermute vec $
                                  foldr (V.++) V.empty
                                  [rawIdxG1,rawIdxG2,norIdxG1
                                  ,norIdxG2]) 

                             ,vec `at` gsIdx)
      tabHeader isUp = let (minI,maxI) = findNumPart header
                           ls = [0..V.length header - 1]
                           begPart = V.toList $ V.unsafeBackpermute header $
                                      V.fromList $ take minI ls
                           annPart = V.toList $ V.unsafeBackpermute header $
                                      V.fromList $ drop (maxI + 1) ls
                           fcStrs = map (render . setManyAttrib [("s1",gs1),("s2",gs2)]) 
                                     [afcTemplate,rgTemplate]
                           nA = length annPart
                           gss = [render $ setAttribute "g" gs1 grawTemplate
                                 ,render $ setAttribute "g" gs2 grawTemplate
                                 ,render $ setAttribute "g" gs1 gnorTemplate
                                 ,render $ setAttribute "g" gs2 gnorTemplate
                                 ]
                           nB = length $ begPart ++ ["P-value"] ++ fcStrs ++
                                annPart ++
                                gss
                           titleLs = begPart ++ ["P-value"] ++ fcStrs ++
                                     annPart ++
                                     gss ++
                                     map (header `at`)
                                     (V.toList $ foldr (V.++) V.empty [rawIdxG1,rawIdxG2,norIdxG1,norIdxG2])
                           len = length titleLs
                           regStr = if isUp then "up" else "down"
                           titleS = if isUp then "upTitle" else "dnTitle"
                           pConStr =
                             case tCon of
                               Paired -> "paired"
                               _      -> "unpaired"
                           attrs  = let commonAttr = [("gName1",gs1)
                                                     ,("gName2",gs2)
                                                     ,("pCutOff" ,B8.pack $ printf "%.2f" pCutOff)
                                                     ,("fcCutOff",B8.pack $ printf "%.1f" fcCutOff)
                                                     ,("pairOrUnpair",pConStr)
                                                     ,("rawBeg", toStr $ length begPart+7+nA)
                                                     ,("rawEnd", toStr $ length begPart+6+n1+n2+nA)
                                                     ,("norBeg", toStr $ length begPart+7+n1+n2+nA)
                                                     ,("norEnd", toStr $ length begPart+6+2*(n1+n2)+nA)
                                                     ,("annBeg", toStr $ length begPart+3)
                                                     ,("annEnd", toStr $ length begPart+2+nA)
                                                     ,("grBeg", toStr $ nB - 4)
                                                     ,("grEnd", toStr $ nB - 3)
                                                     ,("gnBeg", toStr $ nB - 2)
                                                     ,("gnEnd", toStr $ nB - 1)
                                                     ,("annos", foldl1 (\a b -> a `B8.append` ", " `B8.append` b) annPart)]
                                    in case rna of
                                      Coding    -> commonAttr
                                      NonCoding ->
                                        let source = toStr $ fromJust $ elemIndex "source" titleLs 
                                            relaBeg = toStr $ fromJust $ elemIndex "relationship" titleLs
                                            relaEnd = toStr $ nB - length gss - 1
                                        in commonAttr ++ 
                                           [("source",source)
                                           ,("relaBeg",relaBeg)
                                           ,("relaEnd",relaEnd)]
                           noteStr = render $
                                     setManyAttrib attrs $
                                     groupTemplate setting
                           noteCell = string noteStr
                                      # mergeAcross (fromIntegral $ len - 1)
                                      # mergeDown idxLen
                                      # withStyleID "noteCell"
                                      # addTextPropertyAtRanges (noteStr `match` vsRegex) [Bold, Text $ dfp {color = Just red}]
                                      # addTextPropertyAtRanges (noteStr `match` cutOffRegex) [Bold, Text $ dfp {color = Just red}] 
                                      # addTextPropertyAtRanges (noteStr `match` log2Regex) [Bold, Text $ dfp {color= Just dodgerblue}] 
                           note = mkRow [noteCell]
                           mol = case chip of
                             GE -> "genes"
                             _  -> case rna of
                               Coding -> "mRNAs"
                               _      -> "LncRNAs"
                           line1 = mkRow
                                   [withStyleID titleS $
                                    mergeAcross (fromIntegral $ len - 1) $
                                    string $ B8.unpack $ render $
                                    setManyAttrib [("fc", B8.pack $ printf "%.1f" fcCutOff)
                                                  ,("s1",gs1)
                                                  ,("s2",gs2)
                                                  ,("reg",regStr)
                                                  ,("mol",mol)] tabHeaderTemplate
                                   ]
                           line2 = mkRow
                                   [emptyCell
                                   ,string "P-value" # withStyleID "title"
                                   ,string "Fold Change and Regulation" # mergeAcross 1 # withStyleID "frCell"
                                   ,string "Annotations" # mergeAcross (fromIntegral $ length annPart - 1) # withStyleID "annoCell"
                                   ,string "Group Raw Intensity" # mergeAcross 1 # withStyleID "groupRawCell"
                                   ,string "Group Normalized Intensity" # mergeAcross 1 # withStyleID "groupNorCell"
                                   ,string "Raw Intensities" # mergeAcross (fromIntegral $ n1+n2-1) # withStyleID "riCell"
                                   ,string "Normalized Intensities" # mergeAcross (fromIntegral $ n1+n2-1) # withStyleID "niCell"
                                   ]
                           line3 = mkRow $
                                   map (withStyleID "boldCell" . string . B8.unpack) titleLs
                           idxLen = fromIntegral $ length (filter (== '\n') $ groupStr setting) + 2  -- 首尾各空一行                                 
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
            (unzip $ map (mkRowIdx True) a
            ,unzip $ map (mkRowIdx False) b)) (ups,dns)
      upSheet = let (hs,begIdx) = tabHeader True
                in mkWorksheet (Name $ B8.unpack gs1 ++ " vs " ++ B8.unpack gs2 ++ "_up") $
                   mkTable $ hs ++ zipWith (\idx row -> row # begAtIdx idx) [begIdx..] upRows
      dnSheet = let (hs,begIdx) = tabHeader False
                in mkWorksheet (Name $ B8.unpack gs1 ++ " vs " ++ B8.unpack gs2 ++ "_down") $
                   mkTable $ hs ++ zipWith (\idx row -> row # begAtIdx idx) [begIdx..] dnRows
  in ((upSheet,upGs),(dnSheet,dnGs))
  

