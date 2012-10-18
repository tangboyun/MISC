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

module Wrapper where

import qualified Data.ByteString.Lazy.Char8 as B8
import Data.ByteString.Lazy (ByteString)
import           Text.XML.SpreadsheetML.Writer
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Builder
import AllTargets
import DiffExp
import UtilFun
import System.IO
import Types
import Control.Arrow ((&&&))
import Control.Applicative
import Control.Exception

toATVWB :: Setting -> FilePath  -> FilePath -> FilePath -> IO ()
toATVWB s inFile outFile logFile =
  do
    _ <- bracket (openFile inFile ReadMode) (hClose)
         (\h1 ->
           do
             (wb,_) <- hToATVWB s h1
             _ <- bracket (openFile outFile WriteMode) (hClose) $
                  (\h2 ->
                    hPutStr h2 $ showSpreadsheet wb)
             return ()
         )
    _ <- bracket (openFile inFile ReadMode) (hClose)
         (\h1 ->
           do
             (_,gs) <- hToATVWB s h1
             _ <- bracket
                  (openFile logFile AppendMode)
                  (hClose)
                  (\h3 ->
                    B8.hPutStrLn h3 $ B8.unlines gs)
             return ()
         )
    return ()


hToATVWB :: Setting -> Handle -> IO (Workbook, [ByteString])
hToATVWB setting h =
  fmap (allTargetWB setting) (B8.hGetContents h)

fToSheet :: CutOff -> Setting -> Fun -> FilePath -> (ByteString,ByteString) -> IO ((Worksheet,[ByteString]),(Worksheet,[ByteString]))
fToSheet c s f fp p =
  fmap (flip (f c s) p . preprocess s . parseTSV s) (B8.readFile fp)

fToSheets :: CutOff -> Setting -> Fun -> FilePath -> [(ByteString,ByteString)] -> IO [Worksheet]
fToSheets c s f fp =
   fmap concat . mapM (fmap (uncurry (\a b -> [fst a,fst b])) . fToSheet c s f fp)
   
fToReport :: CutOff -> Setting -> Fun -> FilePath -> [(ByteString,ByteString)] -> IO Workbook
fToReport c s f fp = 
   fmap (addStyles . mkWorkbook) . fToSheets c s f fp

fToGeneList :: CutOff -> Setting -> Fun -> FilePath -> [(ByteString,ByteString)] -> IO [(ByteString,[ByteString])]
fToGeneList c s f fp ps = 
  fmap concat $
  mapM (\p@(s1,s2) ->
         let str = s1 `B8.append` " vs " `B8.append` s2
             upStr = str `B8.append` "_up"
             dnStr = str `B8.append` "_down"
         in fmap (uncurry (\a b -> [(upStr,snd a),(dnStr,snd b)])) $
            fToSheet c s f fp p) ps

fToFCGeneList, fToVPGeneList :: CutOff -> Setting -> FilePath -> [(ByteString,ByteString)] -> IO [(ByteString,[ByteString])]
fToFCGeneList c s = fToGeneList c s sampleSheet
fToVPGeneList c s = fToGeneList c s groupSheet

fToFCReport, fToVPReport :: CutOff -> Setting -> FilePath -> [(ByteString,ByteString)] -> IO Workbook
fToFCReport c s = fToReport c s sampleSheet
fToVPReport c s fp ps =
  let vpSheet = volcanoPlotSheet c s
  in fmap
     (\wb ->
       wb { workbookWorksheets = vpSheet : workbookWorksheets wb}) $
     fToReport c s groupSheet fp ps

fToHybGeneList :: CutOff -> Setting -> FilePath -> (GroupPairs,SamplePairs) -> IO [(ByteString,[ByteString])]
fToHybGeneList c s fp (G gs, S ss) =
  (++) <$> fToVPGeneList c s fp gs
       <*> fToFCGeneList c s fp ss
  
fToHybReport :: CutOff -> Setting -> FilePath -> (GroupPairs,SamplePairs) -> IO Workbook
fToHybReport c s fp (G gs, S ss) =
  let vpSheet = volcanoPlotSheet c s
  in fmap
     (addStyles . mkWorkbook . (vpSheet :)) $
     (++) <$> fToSheets c s groupSheet fp gs
          <*> fToSheets c s sampleSheet fp ss

