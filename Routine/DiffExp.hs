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
import qualified Data.Vector as V
import           Text.StringTemplate
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Writer

data CutOff = C
              !Double -- fold change
              !(Maybe Double) -- p

parseTSV :: ByteString -> (V.Vector ByteString,[V.Vector ByteString])
parseTSV =
  (\ls -> (V.fromList $ head ls,map V.fromList $ tail ls)) .
  map (B8.split '\t') .
  filter ((/= '#') .  B8.head) . B8.lines



toCol :: Int -> ByteString
toCol i =
  let ls = ['A'..'Z']
      (x,y) = i `divMod` 26
  in if x > 0
     then B8.cons' (ls !! (x-1)) $! B8.singleton $! ls !! y
     else B8.singleton $! ls !! i

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf = B8.isPrefixOf `on` B8.reverse

extractG = B8.tail . head . tail . B8.split ',' . B8.takeWhile (/= ']') . B8.tail
extractS = head . B8.split ',' . B8.takeWhile (/= ']') . B8.tail

sampleSheet :: CutOff -> (V.Vector ByteString,[V.Vector ByteString]) -> (ByteString,ByteString) -> Worksheet
sampleSheet (C f _) (header,vecs) (s1,s2) =
  let rawIdxs = V.findIndices ("(raw)" `isSuffixOf`) header
      norIdxs = V.findIndices ("(normalized)" `isSuffixOf`) header
      at = V.unsafeIndex
      rawIdxS1 = fromJust $ V.findIndex ((== s1). extractS . (header `at`)) rawIdxs
      rawIdxS2 = fromJust $ V.findIndex ((== s2). extractS . (header `at`)) rawIdxs
      norIdxS1 = fromJust $ V.findIndex ((== s1). extractS . (header `at`)) norIdxs
      norIdxS2 = fromJust $ V.findIndex ((== s2). extractS . (header `at`)) norIdxs
      fc i j vec = let v = (read $! B8.unpack $! vec `at` i) - (read $! B8.unpack $! vec `at` j)
                   in (signum v * (2 ** (abs v)),vec)
  in undefined 
  
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
sampleTemplate =
  newSTMP "# Fold Change cut-off: $fcCutOff$\n\
  \# Condition pairs:  $sample1$ vs $sample2$\n\
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
