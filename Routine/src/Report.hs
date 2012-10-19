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
import Text.XML.SpreadsheetML.Types hiding (Formula)
import qualified Data.ByteString.Lazy.Char8 as B8

import System.FilePath
import System.Directory
import Control.Monad
import Types
import AllTargets
import UtilFun
import Wrapper

mkdir fp = doesDirectoryExist fp >>=
           flip unless (createDirectory fp) 

sp = [("C","N")
     ,("F", "N")
     ,("F", "C")
     ]
  
main :: IO ()
main = do
  mkdir "result" >> mkdir "GoGene" >> mkdir "PathGene"
  inPut:_ <- getArgs
--  inStr <- B8.readFile inPut
  let setting = Setting Lnc Coding Rat Formula
      cutOff = C 1.5 (Just (Unpaired,0.05))
--      (allTarget,infos) = allTargetWB setting inStr
  (allTarget,infos) <- fToATVWB setting inPut
  writeFile "All.xml" $ showSpreadsheet allTarget
  
  wb <- fToVPReport cutOff setting inPut sp
  writeFile "DEG.xml" $ showSpreadsheet wb
  --    x = parseTSV setting inStr
  -- writeFile ("result" </> "All.xml") $ showSpreadsheet allTarget
  -- B8.appendFile ("result" </> "note.txt") $ B8.unlines infos
  -- writeFile ("result" </> "DEG.xml") $ showSpreadsheet $ mkVPReport cutOff setting x sp
  -- forM_ (mkVPGeneList cutOff setting x sp) $ \(str,gs) -> do
  --   let gs' = filter (not . B8.null) gs
  --   B8.writeFile ("GoGene" </> B8.unpack str <.> "txt") $ B8.unlines gs'
  --   B8.appendFile ("result" </> "note.txt") $ str `B8.append` "\t" `B8.append` (B8.pack $ show $ length gs) `B8.append` "\n"
  --   if ("_up" `isSuffixOf` str) 
  --     then 
  --       B8.writeFile ("PathGene" </> B8.unpack str <.> "txt") $ B8.unlines $ map (`B8.append` "\torange") gs'
  --     else 
  --       B8.writeFile ("PathGene" </> B8.unpack str <.> "txt") $ B8.unlines $ map (`B8.append` "\tyellow") gs'
