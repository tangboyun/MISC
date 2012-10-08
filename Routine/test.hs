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
import System.Environment
import DiffExp
import           Text.XML.SpreadsheetML.Writer (showSpreadsheet)
import           Text.XML.SpreadsheetML.Builder
import Text.XML.SpreadsheetML.Types
import qualified Data.ByteString.Lazy.Char8 as B8
import System.FilePath
import System.Directory
import Control.Monad

mkdir fp = doesDirectoryExist fp >>=
           flip unless (createDirectory fp) 

ps = [("1C", "1B")
     ,("1B", "1A")
     ,("1C", "1D")
     ,("1D", "1A")
     ,("1D", "1B")
     ,("1C", "1A")
     ,("2C", "2B")
     ,("2B", "2A")
     ,("2C", "2D")
     ,("2D", "2A")
     ,("2D", "2B")
     ,("2C", "2A")
     ,("3C", "3B")
     ,("3B", "3A")
     ,("3C", "3D")
     ,("3D", "3A")
     ,("3D", "3B")
     ,("3C", "3A")
     ,("4C", "4B")
     ,("4B", "4A")
     ,("4C", "4D")
     ,("4D", "4A")
     ,("4D", "4B")
     ,("4C", "4A")]
  
main :: IO ()
main = do
  mkdir "result" 
  mkdir "GoGene" 
  mkdir "PathGene"
  inPut:outPut:_ <- getArgs
  inStr <- B8.readFile inPut
  writeFile ("result" </> outPut) $ showSpreadsheet $ mkFCWorkbook (C 1.5 Nothing) (parseTSV inStr) ps
  forM_ (mkDEGList (C 1.5 Nothing) (parseTSV inStr) ps) $ \(str,gs) -> do
    let gs' = filter (not . B8.null) gs
    B8.writeFile ("GoGene" </> B8.unpack str <.> "txt") $ B8.unlines gs'
    B8.appendFile ("result" </> "note.txt") $ str `B8.append` "\t" `B8.append` (B8.pack $ show $ length gs) `B8.append` "\n"
    if ("_up" `isSuffixOf` str) 
      then 
        B8.writeFile ("PathGene" </> B8.unpack str <.> "txt") $ B8.unlines $ map (`B8.append` "\torange") gs'
      else 
        B8.writeFile ("PathGene" </> B8.unpack str <.> "txt") $ B8.unlines $ map (`B8.append` "\tyellow") gs'
