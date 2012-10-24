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

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.IORef
import           Graphics.UI.Gtk
import           Report.GUI.Layout
import           Report.Sheet
import           Report.Sheet.UtilFun
import           Report.Types
import           System.Directory
import           System.FilePath
import           Text.Printf
import           Text.XML.SpreadsheetML.Writer (showSpreadsheet)

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window [windowTitle := "Expression Profiling Report Maker",
              windowDefaultWidth := 300, 
              containerBorderWidth := 30 ]
  refFC <- newIORef Nothing
  refVP <- newIORef Nothing
  topVBox <- vBoxNew False 20
--  pBox <- projectBox
  (cBox,(refChip,refProbe,refSp)) <- chipBox
  (iBox,iChooser) <- inputBox
  (oBox,oChooser) <- outputBox
  
  aBox <- hBoxNew False 0
  rB <- buttonNewFromStock stockApply
  boxPackStart aBox rB PackRepel 0
    
  
  tBox <- filterBox refFC refVP iChooser
  sep <- hSeparatorNew
  containerAdd window topVBox
--  containerAdd topVBox pBox
  containerAdd topVBox cBox
  containerAdd topVBox iBox
  containerAdd topVBox oBox
  containerAdd topVBox tBox  
  containerAdd topVBox sep
  containerAdd topVBox aBox

  onClicked rB $ do
    fcSetting <- readIORef refFC
    vpSetting <- readIORef refVP
    chip <- readIORef refChip
    rna <- readIORef refProbe
    sp <- readIORef refSp
    minFile <- fileChooserGetFilename iChooser
    moutPath <- fileChooserGetFilename oChooser
    let setting = Setting chip rna sp Numeric
    case minFile of
      Nothing -> return ()
      Just inFile -> case moutPath of
        Nothing -> return ()
        Just outPath -> do
          mkATVSpreadSheet setting inFile outPath
          mkDEGSpreadSheet setting (fcSetting,vpSetting) inFile outPath
          
  widgetShowAll window 
  
  onDestroy window mainQuit
  mainGUI  


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
                                "P-value cut-off: " ++ printf "%.1f" p ++ "\n"
      degWB <- fun1 cutOff s inFile ps'
      writeFile (outPath </> degFile bool s) $ showSpreadsheet degWB
      B8.appendFile (outPath </> noteFile ) $ B8.pack cutOffStr
      degGs <- fun2 cutOff s inFile ps'
      mkDEGListFiles outPath degGs
    go2 (vpC@(C fc1 (Just (_,p))),fcC@(C fc2 _)) (fun1,fun2) (gs,ss) = do
      let f = map (\(a,b) -> (B8.pack a,B8.pack b))
          gs' = G $ f gs
          ss' = S $ f ss
          cutOffStr = "\n\nFold Change cut-off(Sample vs Sample): " ++ printf "%.2f" fc2 ++ "\n" ++
                      "Fold Change cut-off(Group vs Group): " ++ printf "%.1f" fc1 ++ "\n" ++
                      "P-value cut-off: " ++ printf "%.1f" p ++ "\n"          
      degWB <- fun1 (vpC,fcC) s inFile (gs',ss')
      writeFile (outPath </> degFile True s) $ showSpreadsheet degWB
      B8.appendFile (outPath </> noteFile ) $ B8.pack cutOffStr      
      degGs <- fun2 (vpC,fcC) s inFile (gs',ss')
      mkDEGListFiles outPath degGs

mkATVSpreadSheet :: Setting -> FilePath -> FilePath -> IO ()
mkATVSpreadSheet s inFile outPath = do
  (atvWB,_) <- fToATVWB s inFile
  writeFile (outPath </> atvFile s) $ showSpreadsheet atvWB
  (_,infos) <- fToATVWB s inFile
  B8.appendFile (outPath </> noteFile ) $ B8.unlines infos


goFolder :: FilePath
goFolder = "GO"

pathwayFolder :: FilePath
pathwayFolder = "Pathway"

mkDEGListFiles :: FilePath -> [(B8.ByteString,[B8.ByteString])] -> IO ()
mkDEGListFiles ofp ps =
  let goOut = ofp </> goFolder
      pathOut = ofp </> pathwayFolder
  in do
    mkdir goOut >> mkdir pathOut
    forM_ ps $ \(str,gs) -> do
      let gs' = filter (not . B8.null) gs
      B8.writeFile (goOut </> B8.unpack str <.> "txt") $
        B8.unlines gs'
      B8.appendFile (ofp </> noteFile ) $
        str `B8.append` "\t" `B8.append`
        (B8.pack $ show $ length gs) `B8.append` "\n"
      if ("_up" `isSuffixOf` str) 
        then B8.writeFile (pathOut </> B8.unpack str <.> "txt") $
             B8.unlines $ map (`B8.append` "\torange") gs'
        else B8.writeFile (pathOut </> B8.unpack str <.> "txt") $
             B8.unlines $ map (`B8.append` "\tyellow") gs'

