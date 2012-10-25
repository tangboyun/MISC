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

module Report.Sheet where

import           Control.Applicative
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Report.Sheet.ATVSheet
import           Report.Sheet.DEGSheet
import           Report.Sheet.UtilFun
import           Report.Types
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types



fToATVWB :: Setting -> FilePath -> IO (Workbook, [ByteString])
fToATVWB setting fp =
  fmap (allTargetWB setting) (readFile' fp)

fToSheet :: CutOff -> Setting -> Fun -> FilePath -> (ByteString,ByteString) -> IO ((Worksheet,[ByteString]),(Worksheet,[ByteString]))
fToSheet c s f fp p =
  fmap (flip (f c s) p . preprocess s . parseTSV s) (readFile' fp)

fToSheets :: CutOff -> Setting -> Fun -> FilePath -> [(ByteString,ByteString)] -> IO [Worksheet]
fToSheets c s f fp =
   fmap concat . mapM (fmap (uncurry (\(a,_) (b,_) -> [a, b])) . fToSheet c s f fp)
   
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
         in fmap (uncurry (\(_,a) (_,b) -> [(upStr, a),(dnStr, b)])) $
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

fToHybGeneList :: (CutOff,CutOff) -> Setting -> FilePath -> (GroupPairs,SamplePairs) -> IO [(ByteString,[ByteString])]
fToHybGeneList (cg,cs) s fp (G gs, S ss) =
  (++) <$> fToVPGeneList cg s fp gs
       <*> fToFCGeneList cs s fp ss
  
fToHybReport :: (CutOff,CutOff) -> Setting -> FilePath -> (GroupPairs,SamplePairs) -> IO Workbook
fToHybReport (cg,cs) s fp (G gs, S ss) =
  let vpSheet = volcanoPlotSheet cg s
  in fmap
     (addStyles . mkWorkbook . (vpSheet :)) $
     (++) <$> fToSheets cg s groupSheet fp gs
          <*> fToSheets cs s sampleSheet fp ss

readFile' = fmap (B8.filter (/= '\r')) . B8.readFile 
