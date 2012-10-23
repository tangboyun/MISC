{-# LANGUAGE OverloadedStrings, BangPatterns #-}
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
module UtilFun where

import           Control.Arrow ((&&&))
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Char
import           Data.Function
import           Data.List hiding (isSuffixOf)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import qualified Data.Vector as V
import           Text.Regex.Posix
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Types
import           Statistics.Sample
import           Statistics.Distribution
import           Statistics.Distribution.StudentT
import           Text.Regex.Posix

ttestUnpaired,ttestPaired :: V.Vector Double -> V.Vector Double -> Double

ttestUnpaired !v1 !v2 | V.length v1 > 1, V.length v2 > 1 =
  let (m1,var1) = meanVarianceUnb v1
      (m2,var2) = meanVarianceUnb v2
      n1 = fromIntegral $! V.length v1
      n2 = fromIntegral $! V.length v2
      df = n1 + n2 - 2
      s_pooled = sqrt $! (var1 * (n1-1) + var2 * (n2-1)) / df
      s = s_pooled * (sqrt $! 1 / n1 + 1 / n2) 
      t = (m1 - m2) / s
      dis = studentT df
  in 2 * cumulative dis (negate $! abs t)
                      | otherwise = error "Illformed unpaired t-test."
                                    
ttestPaired !v1 !v2 | V.length v1 == V.length v2 =
  let v = V.zipWith (-) v1 v2
      n = fromIntegral $ V.length v
      df = n - 1
      (m,var) = meanVarianceUnb v
      t = m / (sqrt var / sqrt n)
      dis = studentT df
  in 2 * cumulative dis (negate $! abs t)
                    | otherwise = error "unequal sample size."

preprocess :: Setting -> (V.Vector ByteString,[V.Vector ByteString]) -> (V.Vector ByteString,[V.Vector ByteString])
preprocess setting (h,vecs) =
  let (header:vs) = removeUnusedAnno setting $ 
                    reorganize $ -- 保证样品与组是连续排列的
                    V.map removeDQ h: vecs
  in (header,vs)


removeUnusedAnno :: Setting -> [V.Vector ByteString] -> [V.Vector ByteString]
removeUnusedAnno _ [] = []
removeUnusedAnno (Setting c r _ _) rs@(h:rows) =
  case c of
    GE -> 
      case V.findIndex (=~ B8.pack "\\s*[Cc]ontrol.*[Tt]ype\\s*") h of
        Nothing -> rs
        Just i  -> map (V.ifilter (\idx _ -> idx /= i)) $
                   h : filter (\vec -> vec `V.unsafeIndex` i == "false" ) rows
    _ ->
      case r of
        NonCoding ->
          let as = findAnnPart h  
              aIdxs = V.unsafeBackpermute as $
                      V.findIndices (`notElem` lncRemoveList) $
                      V.unsafeBackpermute h as
              lncRemoveList = ["Cytoband"
                              ,"Description"
                              ,"EnsemblID"
                              ,"EntrezGene"
                              ,"GO(Avadis)"
                              ,"TIGRID"
                              ,"GeneSymbol"
                              ,"UniGene"
                              ]
              idxVec = V.fromList $ [0..snd (findNumPart h)] ++ V.toList aIdxs
          in map (flip V.unsafeBackpermute idxVec) rs
        _ -> rs  
      
    


match :: String -> String -> [(Int,Int)]
match str regex = map (\(a,b) -> (a, a+b)) $ getAllMatches $ str =~ regex

findNumPart = (V.minimum &&& V.maximum) .
              V.findIndices (\e ->
                              "(raw)" `isSuffixOf` e ||
                              "(normalized)" `isSuffixOf` e
                            )
findNumPart' = (V.minimum &&& V.maximum) .
               V.findIndices (\e ->
                               "(raw)" `isSuffixOf'` e ||
                               "(normalized)" `isSuffixOf'` e
                             )
removeDQ str = if B8.head str == '"'
               then B8.tail $ B8.init str
               else str


findRawPart = V.findIndices ("(raw)" `isSuffixOf`)
findRawPart' = V.findIndices ("(raw)" `isSuffixOf'`)
findNorPart = V.findIndices ("(normalized)" `isSuffixOf`)
findNorPart' = V.findIndices ("(normalized)" `isSuffixOf'`)
findAnnPart vec = V.fromList [(snd $ findNumPart vec) + 1..V.length vec - 1]
findAnnPart' vec = V.fromList [(snd $ findNumPart' vec) + 1..V.length vec - 1]
reorganize :: [V.Vector ByteString] -> [V.Vector ByteString]
reorganize [] = []
reorganize (h:rs) =
  let at = V.unsafeIndex
      (rawBeg,norEnd) = findNumPart h
      organize vec = sortBy (\a b ->
                              comparing (extractG . (h `at`)) a b `mappend`
                              comparing (extractS . (h `at`)) a b
                            ) $ V.toList vec
      !idxVec = V.fromList $ take (V.length h) $
                [0..rawBeg-1] ++ organize (findRawPart h) ++
                organize (findNorPart h) ++ [norEnd + 1..]
  in map (\e ->
           let !e' = V.unsafeBackpermute e idxVec
           in e'
         ) (h:rs)
      
parseATheads :: [String] -> String
parseATheads = 
     let regex = "[^0-9]*([0-9]+) out of ([0-9]+).*" :: String
     in tail . head . head . filter (not . null) . map (getAllTextSubmatches . (=~ regex))
                  
parseTSV :: Setting -> ByteString -> (V.Vector ByteString,[V.Vector ByteString])
parseTSV (Setting _ rna _ _) str =
  (\ls ->
    let i = fromJust $ V.elemIndex "Number Passed" h
        j = fromJust $ V.elemIndex "GeneSymbol" h
        h =  V.fromList $ head ls
        f = case rna of
              Coding -> V.ifilter (\idx _ -> idx /= i)
              _      -> V.ifilter (\idx _ -> idx /= i && idx /= j)
    in (f h,map (f . V.fromList) $ tail ls)) $
  map (B8.split '\t') $
  filter ((/= '#') .  B8.head) $ B8.lines str

toCell :: String -> Cell  
toCell str = if all isDigit str && not (null str)
             then number $ read str
             else string str

toStr :: Int -> ByteString
toStr i =
  let ls = ['A'..'Z']
      (x,y) = i `divMod` 26
  in if x > 0
     then B8.cons' (ls !! (x-1)) $! B8.singleton $! ls !! y
     else B8.singleton $! ls !! i
          
toStr' :: Int -> S.ByteString
toStr' = B8.toStrict . toStr

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf = B8.isPrefixOf `on` B8.reverse

isSuffixOf' :: S.ByteString -> S.ByteString -> Bool
isSuffixOf' = isSuffixOf `on` B8.fromStrict

extractG = B8.tail . head . tail . B8.split ',' . B8.takeWhile (/= ']') . B8.tail
extractG' = B8.toStrict . extractG . B8.fromStrict
extractS = head . B8.split ',' . B8.takeWhile (/= ']') . B8.tail
extractS' = B8.toStrict . extractS . B8.fromStrict
