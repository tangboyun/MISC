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
import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Report.Sheet.ATVSheet
import           Report.Sheet.DEGSheet
import           Report.Sheet.UtilFun
import           Report.Types
import           System.Directory
import           System.FilePath
import           Text.Printf
import           Text.XML.SpreadsheetML.Builder hiding (bool)
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Writer (showSpreadsheet)



fToATVWB :: Setting -> FilePath -> IO (Workbook, [ByteString])
fToATVWB setting fp =
  fmap (allTargetWB setting) (readFile' fp)

fToSheet :: CutOff -> Setting -> Fun -> FilePath -> (ByteString,ByteString) -> IO ((Worksheet,[ByteString]),(Worksheet,[ByteString]))
fToSheet c s f fp p =
  fmap (flip (f c s) p . preprocess s . parseTSV) (readFile' fp)

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


noteFile :: FilePath
noteFile = "note.txt"

degFile :: Bool -> Setting -> FilePath
degFile isVP (Setting chip rna _ _) = "Differentially Expressed "++ specStr ++ vpStr ++ ".xml"
  where vpStr = if isVP then " (Pass Volcano Plot)" else ""
        specStr =
          case chip of
            GE -> "Genes"
            _ -> case rna of
              Coding -> "mRNAs"
              _      -> "LncRNAs"

atvFile :: Setting -> FilePath
atvFile (Setting chip rna _ _) = specStr ++ commonStr
  where commonStr = " Expression Profiling Data.xml"
        specStr =
          case chip of
            GE -> "Gene"
            _ -> case rna of
              Coding -> "mRNA"
              _      -> "LncRNA"


mkdir fp = doesDirectoryExist fp >>=
           flip unless (createDirectory fp) 

mkDEGSpreadSheet :: Setting -> (Maybe FSet, Maybe FSet) -> FilePath -> FilePath -> IO ()
mkDEGSpreadSheet s (fcS,vpS) inFile outPath = do
  case vpS of
    Nothing ->
      case fcS of
        Nothing -> return ()
        Just (F fcCutOff samlePairs) -> go1 fcCutOff (fToFCReport,fToFCGeneList) samlePairs              
    Just (F vpCutOff groupPairs) ->
      case fcS of
        Nothing -> go1 vpCutOff (fToVPReport,fToVPGeneList) groupPairs
        Just (F fcCutOff samlePairs) -> go2 (vpCutOff,fcCutOff) (fToHybReport,fToHybGeneList) (groupPairs,samlePairs)
  where 
    go1 cutOff (fun1,fun2) ps = do
      let ps' = map (\(a,b) -> (B8.pack a,B8.pack b)) ps
          bool = case cutOff of
                   C _ Nothing -> False
                   _           -> True
          cutOffStr = case cutOff of
            C fc Nothing -> "\n\nFold Change cut-off: " ++ printf "%.2f" fc ++ "\n"
            C fc (Just (_,p)) -> "\n\nFold Change cut-off: " ++ printf "%.1f" fc ++ "\n" ++
                                "P-value cut-off: " ++ printf "%.2f" p ++ "\n"
      degWB <- fun1 cutOff s inFile ps'
      writeFile (outPath </> degFile bool s) $ showSpreadsheet degWB
      appendFile (outPath </> noteFile ) cutOffStr
      degGs <- fun2 cutOff s inFile ps'
      mkDEGListFiles s outPath degGs
    go2 (vpC@(C fc1 (Just (_,p))),fcC@(C fc2 _)) (fun1,fun2) (gs,ss) = do
      let f = map (\(a,b) -> (B8.pack a,B8.pack b))
          gs' = G $ f gs
          ss' = S $ f ss
          cutOffStr = "\n\nFold Change cut-off(Sample vs Sample): " ++ printf "%.2f" fc2 ++ "\n" ++
                      "Fold Change cut-off(Group vs Group): " ++ printf "%.1f" fc1 ++ "\n" ++
                      "P-value cut-off: " ++ printf "%.2f" p ++ "\n"          
      degWB <- fun1 (vpC,fcC) s inFile (gs',ss')
      writeFile (outPath </> degFile False s) $ showSpreadsheet degWB
      appendFile (outPath </> noteFile ) cutOffStr      
      degGs <- fun2 (vpC,fcC) s inFile (gs',ss')
      mkDEGListFiles s outPath degGs

mkATVSpreadSheet :: Setting -> FilePath -> FilePath -> IO ()
mkATVSpreadSheet s inFile outPath = do
  (atvWB,_) <- fToATVWB s inFile
  writeFile (outPath </> atvFile s) $ showSpreadsheet atvWB
  (_,infos) <- fToATVWB s inFile
  appendFile (outPath </> noteFile ) $ unlines $ map B8.unpack infos


goFolder :: FilePath
goFolder = "GO"

pathwayFolder :: FilePath
pathwayFolder = "Pathway"

mkDEGListFiles :: Setting -> FilePath -> [(B8.ByteString,[B8.ByteString])] -> IO ()
mkDEGListFiles s ofp ps =
  let goOut = ofp </> goFolder
      pathOut = ofp </> pathwayFolder
  in do
    when (isCoding s) $
      mkdir goOut >> mkdir pathOut
    forM_ ps $ \(str,gs) -> do
      let gs' = filter (not . B8.null) gs
      appendFile (ofp </> noteFile ) $ B8.unpack $
        str `B8.append` "\t" `B8.append`
        (B8.pack $ show $ length gs) `B8.append` "\n"
      when (isCoding s) $ do
        writeFile (goOut </> B8.unpack str <.> "txt") $
          unlines $ map B8.unpack gs'
        if ("_up" `isSuffixOf` str) 
          then writeFile (pathOut </> B8.unpack str <.> "txt") $
               unlines $ map (B8.unpack . (`B8.append` "\torange")) gs'
          else writeFile (pathOut </> B8.unpack str <.> "txt") $
               unlines $ map (B8.unpack . (`B8.append` "\tyellow")) gs'
             
isCoding :: Setting -> Bool
isCoding (Setting chip rna _ _) =
  case chip of
    GE -> True
    _  ->
      case rna of
        Coding -> True
        _      -> False
    
