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
import Data.Char
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
              
data RNA = Coding
         | NonCoding
         deriving (Eq)
                    
data Species = Human
             | Rat
             | Mouse
             deriving (Eq)
                      
defaultS = emptyStyle { fontName = Just "Times New Roman"
                      , fontFamily = Just "Roman"
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



-- fcFormula, afcFormula, lfcFormula :: String
-- fcFormula = "=SIGN(RC[6]-RC[7])*POWER(2,ABS(RC[6]-RC[7]))"
-- afcFormula = "=POWER(2,ABS(RC[4]-RC[5]))"
-- lfcFormula = "=RC[5]-RC[6]"




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

mkDEGList :: CutOff -> (V.Vector ByteString,[V.Vector ByteString]) -> [(ByteString,ByteString)] -> [(ByteString,[ByteString])]
mkDEGList c x = foldr (\p@(s1,s2) acc ->
                        let ((_,gs1),(_,gs2)) = sampleSheet c x p
                            str = s1 `B8.append` " vs " `B8.append` s2
                        in  (str `B8.append` "_up", gs1):
                            (str `B8.append` "_down", gs2):acc
                      ) []
  
mkFCWorkbook :: CutOff -> (V.Vector ByteString,[V.Vector ByteString]) -> [(ByteString,ByteString)] -> Workbook
mkFCWorkbook c x ps = 
  let ls = foldr (\e acc ->
                   let ((ws1,_),(ws2,_)) = sampleSheet c x e
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
                              toCell str = if all isDigit str && not (null str)
                                           then number (read str)
                                           else string str
                              f1 = map (string . B8.unpack) . V.toList
                              f2 = map (number . read . B8.unpack) . V.toList
                              f3 = map (toCell . B8.unpack) . V.toList
                              nor1 = read $ B8.unpack $ vec `at` norIdxS1
                              nor2 = read $ B8.unpack $ vec `at` norIdxS2
                              fcFormula = signum lfcFormula * afcFormula
                              lfcFormula = nor1 - nor2
                              afcFormula = 2 ** abs lfcFormula
                              reg = if isUp then "up" else "down"
                          in (mkRow $ 
                              (f1 $ V.unsafeBackpermute vec headIdx) ++
                              map number [fcFormula,lfcFormula,afcFormula] ++ [string reg] ++
                              (f2 $ V.unsafeBackpermute vec $
                               V.fromList [rawIdxS1,rawIdxS2,norIdxS1
                                          ,norIdxS2]) ++
                              (f3 $ V.unsafeBackpermute vec annoIdx)
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
                                             # mergeDown idxLen
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
                           idxLen = fromIntegral $ (length $ filter (== '\n') sampleStr) + 2  -- 首尾各空一行                                 
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

relationTemplate :: Stringable a => StringTemplate a
relationTemplate = newSTMP relationStr


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

relationStr = "# Columns $relaBeg$ ~ $relaEnd$: the relationship of LncRNA and its nearby coding gene and the coordinate of the coding gene, \
              \including relationship, Associated_gene_acc, Associated_gene_name, Associated_gene_strand, \
              \Associated_gene_start, Associated_gene_end.\n\
              \\"sense_overlapping\": the LncRNA's exon is overlapping a coding transcript exon on the same genomic strand;\n\
              \\"intronic\": the LncRNA is overlapping the intron of a coding transcript on the same genomic strand;\n\
              \\"natural antisense\": the LncRNA is transcribed from the antisense strand and overlapping with a coding transcript; \n\
              \\"non-overlapping antisense\": the LncRNA is transcribed from the antisense strand without sharing overlapping exons;\n\
              \\"bidirectional\": the LncRNA is oriented head to head to a coding transcript within 1000 bp;\n\
              \\"intergenic\": there are no overlapping or bidirectional coding transcripts nearby the LncRNA."

source s = case s of
  Human -> insert humanSource
  Mouse -> insert mouseSource
  _     -> ""
  where
    insert c =
      "RefSeq_NR: RefSeq validated non-coding RNA;\n\
      \UCSC_knowngene: UCSC known genes annotated as \"non-coding\", \"near-coding\" and \"antisense\" \
      \(http://genome.ucsc.edu/cgi-bin/hgTables/);\n\
      \Ensembl: Ensembl (http://www.ensembl.org/index.html);\n" ++ c ++
      "RNAdb: RNAdb2.0 (http://research.imb.uq.edu.au/rnadb/);\n\
      \NRED: NRED (http://jsm-research.imb.uq.edu.au/nred/cgi-bin/ncrnadb.pl);\n\
      \UCR: \"ultra-conserved region\" among human, mouse and rat (http://users.soe.ucsc.edu/~jill/ultra.html);\n\
      \lincRNA: lincRNA identified by John Rinn's group (Guttman et al. 2009; Khalil et al. 2009);\n\
      \misc_lncRNA: other sources."
    humanSource = "H-invDB: H-invDB (http://www.h-invitational.jp/);\n"
    mouseSource = "Fantom: Fantom project (http://fantom.gsc.riken.jp/);\n"

mkComment = undefined
mkHeadLine = undefined
