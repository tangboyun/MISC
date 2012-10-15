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
module UtilFun where

import Control.Arrow ((&&&))
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Text.Regex.Posix 
import qualified Data.Vector as V
import           Data.Function
import           Types
import           Data.Maybe
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Data.Char

match :: String -> String -> [(Int,Int)]
match str regex = map (\(a,b) -> (a, a+b)) $ getAllMatches $ str =~ regex

parseATheads :: [String] -> String
parseATheads = 
     let regex = "[^0-9]*([0-9]+) out of ([0-9]+).*" :: String
     in tail . head . head . filter (not . null) . map (getAllTextSubmatches . (=~ regex))
                  
parseTSV :: ByteString -> Setting -> (V.Vector ByteString,[V.Vector ByteString])
parseTSV str (Setting _ rna _) =
  (\ls ->
    let i = fromJust $ V.findIndex (== "Number Passed") h
        j = fromJust $ V.findIndex (== "GeneSymbol") h
        h =  V.fromList $ head ls
        f = case rna of
              Coding -> V.ifilter (\idx _ -> idx /= i)
              _      -> V.ifilter (\idx _ -> idx /= i && idx /= j)
    in (f h,map (f . V.fromList) $ tail ls)) $
  map (B8.split '\t') $
  filter ((/= '#') .  B8.head) $ B8.lines str

toCell :: String -> Cell  
toCell str = if all isDigit str && not (null str)
             then number (read str)
             else string str

toStr :: Int -> ByteString
toStr i =
  let ls = ['A'..'Z']
      (x,y) = i `divMod` 26
  in if x > 0
     then B8.cons' (ls !! (x-1)) $! B8.singleton $! ls !! y
     else B8.singleton $! ls !! i

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf = B8.isPrefixOf `on` B8.reverse

findNumPart header = (V.minimum &&& V.maximum) $
                     V.findIndices (\e ->
                                     "(raw)" `isSuffixOf` e ||
                                     "(normalized)" `isSuffixOf` e
                                   ) header

extractG = B8.tail . head . tail . B8.split ',' . B8.takeWhile (/= ']') . B8.tail
extractS = head . B8.split ',' . B8.takeWhile (/= ']') . B8.tail
