{-# LANGUAGE OverloadedStrings, BangPatterns #-}
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
import           Data.Function
import Data.Maybe
import Data.List (partition,intercalate)
import qualified Data.Vector as V
import           Text.StringTemplate
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Data.Colour.Names
import Text.Printf
data CutOff = C
              !Double -- fold change
              !(Maybe Double) -- p

defaultS = emptyStyle { fontName = Just "Times New Roman"
                      , fontSize = Just 10
                      }
boldCell = defaultS { fontIsBold = Just True }
title = boldCell { hAlign = Just "Center" }
upTitle = title { bgColor = Just red }
dnTitle = title { bgColor = Just green }
noteCellStyle = defaultS { bgColor = Just khaki
                         , vAlign = Just "Center"
                         }
frCellStyle = title { bgColor = Just orange }
riCellStyle = title { bgColor = Just cornflowerblue }
niCellStyle = title { bgColor = Just lightgreen}
annoCellStyle = title { bgColor = Just lightpink }

parseTSV :: ByteString -> (V.Vector ByteString,[V.Vector ByteString])
parseTSV =
  (\ls ->
    let i = fromJust $ V.findIndex (== "Number Passed") h
        h =  V.fromList $ head ls
        f = V.ifilter (\idx _ -> idx /= i)
    in (f h,map (f . V.fromList) $ tail ls)) .
  map (B8.split '\t') .
  filter ((/= '#') .  B8.head) . B8.lines



toStr :: Int -> ByteString
toStr i =
  let ls = ['A'..'Z']
      (x,y) = i `divMod` 26
  in if x > 0
     then B8.cons' (ls !! (x-1)) $! B8.singleton $! ls !! y
     else B8.singleton $! ls !! i

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf = B8.isPrefixOf `on` B8.reverse

findNumPart header = (\ls -> (V.minimum ls, V.maximum ls)) $
                     V.findIndices (\e ->
                                     "(raw)" `isSuffixOf` e ||
                                     "(normalized)" `isSuffixOf` e
                                   ) header

extractG = B8.tail . head . tail . B8.split ',' . B8.takeWhile (/= ']') . B8.tail
extractS = head . B8.split ',' . B8.takeWhile (/= ']') . B8.tail



fcFormula, afcFormula, lfcFormula :: String
fcFormula = "=SIGN(RC[6]-RC[7])*POWER(2,ABS(RC[6]-RC[7]))"
afcFormula = "=ABS(RC[-2])"
lfcFormula = "=SIGN(RC[-1])*LOG(ABS(RC[-1]),2)"

ttestTemplate, tfcTemplate :: Stringable a => StringTemplate a
ttestTemplate = newSTMP
                "=T.TEST(RC[$rc1Beg$]:RC[$rc1End$],RC[$rc2Beg$]:RC[$rc2End$],2,2)"

tfcTemplate = newSTMP
              "=SUM(RC[$rc3Beg$]:RC[$rc3End$])/SUM(RC[$rc4Beg$]:RC[$rc4End$])"

fcTemplate = newSTMP
             "Fold change([$s1$] vs [$s2$])"
lfcTemplate = newSTMP
              "Log Fold change([$s1$] vs [$s2$])"
afcTemplate = newSTMP
              "Absolute Fold change([$s1$] vs [$s2$])"
rgTemplate = newSTMP
             "Regulation([$s1$] vs [$s2$])"

tabHeaderTemplate = newSTMP "$s1$ vs $s2$ $fc$ fold $reg$ regulated genes"

sampleSheet :: CutOff -> (V.Vector ByteString,[V.Vector ByteString]) -> (ByteString,ByteString) -> ((Worksheet,[ByteString]),(Worksheet,[ByteString]))
sampleSheet (C f _) (header,vecs) (s1,s2) =
  let rawIdxs = V.findIndices ("(raw)" `isSuffixOf`) header
      norIdxs = V.findIndices ("(normalized)" `isSuffixOf`) header
      gsIdx = fromJust $ V.findIndex (== "GeneSymbol") header
      at = V.unsafeIndex
      rawIdxS1 = fromJust $ V.find ((== s1). extractS . (header `at`)) rawIdxs
      rawIdxS2 = fromJust $ V.find ((== s2). extractS . (header `at`)) rawIdxs
      norIdxS1 = fromJust $ V.find ((== s1). extractS . (header `at`)) norIdxs
      norIdxS2 = fromJust $ V.find ((== s2). extractS . (header `at`)) norIdxs
      fc i j vec = let v = (read $! B8.unpack $! vec `at` i) - (read $! B8.unpack $! vec `at` j)
                   in (signum v * (2 ** (abs v)),vec)
      mkRowIdx isUp vec = let (minI,maxI) = findNumPart header
                              ls = [0..V.length header - 1]
                              headIdx = V.fromList $ take minI ls
                              annoIdx = V.fromList $ drop (maxI + 1) ls
                              f1 = map (string . B8.unpack) . V.toList
                              f2 = map (number . read . B8.unpack) . V.toList
                              reg = if isUp then "up" else "down"
                          in (mkRow $ (f1 $ V.unsafeBackpermute vec headIdx) ++
                              map formula [fcFormula,lfcFormula,afcFormula] ++ [string reg] ++
                              (f2 $ V.unsafeBackpermute vec $
                               V.fromList [rawIdxS1,rawIdxS2,norIdxS1
                                          ,norIdxS2]) ++
                              (f1 $ V.unsafeBackpermute vec annoIdx)
                             ,vec `at` gsIdx)
      tabHeader isUp = let (minI,maxI) = findNumPart header
                           ls = [0..V.length header - 1]
                           begPart = V.toList $ V.unsafeBackpermute header $
                                     V.fromList $ take minI ls
                           endPart = V.toList $ V.unsafeBackpermute header $
                                     V.fromList $ drop (maxI + 1) ls
                           fcStr = map (render . setManyAttrib [("s1",s1),("s2",s2)]) $
                                   [fcTemplate,lfcTemplate,afcTemplate,rgTemplate]
                           titleLs = begPart ++ fcStr ++
                                     map (header `at`) [rawIdxS1,rawIdxS2,norIdxS1,norIdxS2] ++
                                     endPart
                           len = length titleLs
                           regStr = if isUp then "up" else "down"
                           titleS = if isUp then "upTitle" else "dnTitle"
                           noteCell = string $ B8.unpack $ render $
                                      setManyAttrib
                                      [("s1",s1)
                                      ,("s2",s2)
                                      ,("fc",B8.pack $ printf "%.1f" f) 
                                      ,("annBeg", toStr $ length begPart + 8)
                                      ,("annEnd", toStr $ len - 1)
                                      ,("annos", foldl1 (\a b -> a `B8.append` ", " `B8.append` b) endPart)]
                                      sampleTemplate
                           note = mkRow
                                   [noteCell # mergeAcross (fromIntegral $ len - 1)
                                             # mergeDown (fromIntegral $ 
                                                          (length $ filter (== '\n') sampleStr) + 5)
                                             # withStyleID "noteCell" ]
                           line1 = mkRow
                                   [withStyleID titleS $
                                    mergeAcross (fromIntegral $ len - 1) $
                                    string $ B8.unpack $ render $
                                    setManyAttrib [("fc", B8.pack $ printf "%.1f" f)
                                                  ,("s1",s1)
                                                  ,("s2",s2)
                                                  ,("reg",regStr)] tabHeaderTemplate
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
                           idxLen = fromIntegral $ (length $ filter (== '\n') sampleStr) + 5                                   
                       in ([note
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
  

groupTemplate :: Stringable a => StringTemplate a
groupTemplate =
  newSTMP "# Fold Change cut-off: $fcCutOff$\n\
  \# P-value cut-off: $pCutOff$\n\
  \# Condition pairs:  $gName1$ vs $gName2$\n\
  \\n\
  \# Column A: ProbeName, it represents probe name.\n\
  \# Column B: P-value, the p-values calculated from $pairOrUnpair$ t-test.\n\
  \# Column C: FC (abs), Absolute Fold change between two groups. \n\
  \# Column D: Regulation, it depicts which group has greater or lower intensity values wrt other group.\n\
  \# Column E, F: Raw intensity of each group.\n\
  \# Column G, H: Normalized intensity of each group (log2 transformed).\n\
  \# Column $rawBeg$ ~ $rawEnd$: Raw intensity of each sample.\n\
  \# Column $norBeg$ ~ $norEnd$: Normalized intensity of each sample (log2 transformed).\n\
  \# Column $annBeg$ ~ $annEnd$: Annotations to each probe, including $annos$."

sampleTemplate :: Stringable a => StringTemplate a
sampleTemplate = newSTMP sampleStr
sampleStr = "# Fold Change cut-off: $fc$\n\
            \# Condition pairs:  $s1$ vs $s2$\n\
            \\n\
            \# Column A: ProbeName, it represents probe name.\n\
            \# Column B: Fold change, positive value indicates up-regulation and negative value indicates down-regulation.\n\
            \# Column C: Log Fold change, log2 value of absolute fold change. \
            \Positive value indicates up-regulation and negative value indicates down-regulation.\n\
            \# Column D: Absolute Fold change between two samples. \n\
            \# Column E: Regulation, it depicts which sample has greater or lower intensity values wrt other sample.\n\
            \# Column F, G: Raw intensity of each sample.\n\
            \# Column H, I: Normalized intensity of each sample (log2 transformed).\n\
            \# Column $annBeg$ ~ $annEnd$: Annotations to each probe, including $annos$."

mkComment = undefined
mkHeadLine = undefined
