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

module GUI where

import Graphics.UI.Gtk hiding (Mouse)
import Graphics.Rendering.Cairo
import Data.IORef
import Control.Applicative
import Data.Maybe
import Data.List
import Types
import Control.Monad

projectBox = do
  projectSetting <- vBoxNew False 10
  projectLine1 <- hBoxNew False 10
  
  custBox <- hBoxNew False 0
  custNameLabel <- labelNew (Just "客户姓名：")
  custName <- entryNew
  boxPackStart custBox custNameLabel PackRepel 0
  boxPackStart custBox custName PackRepel 0

  anaBox <- hBoxNew False 0
  anaNameLabel <- labelNew (Just "分析员：")
  anaName <- entryNew
  boxPackStart anaBox anaNameLabel PackRepel 0
  boxPackStart anaBox anaName PackRepel 0

  
  
  projectIDBox <- hBoxNew False 0
  projectIDLabel <- labelNew (Just "项目编号：")
  projectID <- entryNew 
  boxPackStart projectIDBox projectIDLabel PackNatural 0
  boxPackStart projectIDBox projectID PackNatural 0
  
  boxPackStart projectLine1 custBox PackNatural 0
  boxPackStart projectLine1 projectIDBox PackNatural 0

  projectLine2 <- hBoxNew False 10
  projectTypeLabel <- labelNew (Just "报告类型：")
  projectTypeBox <- hBoxNew False 10
  isMainLand <- radioButtonNewWithLabel "国内报告"
  isI18n <- radioButtonNewWithLabelFromWidget isMainLand "国外报告"
  boxPackStart projectTypeBox isMainLand PackNatural 0
  boxPackStart projectTypeBox isI18n PackNatural 0
  boxPackStart projectLine2 projectTypeLabel PackNatural 0
  boxPackStart projectLine2 projectTypeBox PackNatural 0
  boxPackStart projectLine2 anaBox PackNatural 0


  boxPackStart projectSetting projectLine1 PackNatural 0
  boxPackStart projectSetting projectLine2 PackNatural 0
  return projectSetting

chipBox = do
  cb <- hBoxNew False 20
  chipTypeBox <- hBoxNew False 0
  chipTypeLabel <- labelNew (Just "芯片类型：")
  chipType <- createCBox' ["表达谱芯片", "LncRNA芯片"]
  boxPackStart chipTypeBox chipTypeLabel PackNatural 0
  boxPackStart chipTypeBox chipType PackNatural 0
  refChipType <- newIORef GE
  on chipType changed $ do
    Just str <- comboBoxGetActiveText chipType
    if str == "表达谱芯片"
      then writeIORef refChipType GE
      else writeIORef refChipType Lnc
      
  
  chipSpBox <- hBoxNew False 0
  chipSpLabel <- labelNew (Just "物种：")
  chipSp <- createCBox' ["人类", "小鼠", "大鼠","其他"]
  boxPackStart chipSpBox chipSpLabel PackNatural 0
  boxPackStart chipSpBox chipSp PackNatural 0
  refSp <- newIORef Human
  on chipSp changed $ do
    Just str <- comboBoxGetActiveText chipSp
    case str of
      "人类" -> writeIORef refSp Human
      "小鼠" -> writeIORef refSp Mouse
      "大鼠" -> writeIORef refSp Rat
      "其他" -> writeIORef refSp Other
  

  
  probeTypeBox <- hBoxNew False 0
  probeTypeLabel <- labelNew (Just "探针类型：")
  probeType <- createCBox' ["mRNA", "LncRNA"]
  boxPackStart probeTypeBox probeTypeLabel PackNatural 0
  boxPackStart probeTypeBox probeType PackNatural 0
  refProbe <- newIORef Coding
  on probeType changed $ do
    Just str <- comboBoxGetActiveText probeType
    case str of
      "mRNA" -> writeIORef refProbe Coding
      "LncRNA" -> writeIORef refProbe NonCoding

  
  boxPackStart cb chipTypeBox PackNatural 0
  boxPackStart cb chipSpBox PackNatural 0
  boxPackStart cb probeTypeBox PackNatural 0
  return (cb,(refChipType,refProbe,refSp))
  

inputBox = do
  iBox <- hBoxNew False 10
  inPutLabel <- labelNew (Just "输入文件：")
  input <- fileChooserButtonNew
           "选择输入文件"
           FileChooserActionOpen
  fsfilt <- fileFilterNew
  fileFilterAddPattern fsfilt "*.txt"
  fileFilterSetName fsfilt "All Targets Value (*.txt)"   
  fileChooserAddFilter input fsfilt           

  boxPackStart iBox inPutLabel PackNatural 0
  boxPackStart iBox input PackGrow 0
  return iBox

outputBox = do
  oBox <- hBoxNew False 10
  outPutLabel <- labelNew (Just "输出目录：")  
  outPut <- fileChooserButtonNew "选择输出目录"
            FileChooserActionSelectFolder
          
  boxPackStart oBox outPutLabel PackNatural 0
  boxPackStart oBox outPut PackGrow 0
  return oBox


fcBox refFSet1 refFSet2 = do
  fcBox <- hBoxNew True 10
  bFC <- buttonNewWithMnemonic "_Fold Change Filtering"
  fcAdj1 <- adjustmentNew 2.0  1.0 10.0  0.1  0.5 0
  fcAdj2 <- adjustmentNew 2.0  1.0 10.0  0.1  0.5 0
  pAdj <- adjustmentNew  0.05 0    1.0 0.001 0.01 0
  bVF <- buttonNewWithMnemonic "_Volcano Plot Filtering"  
  onClicked bFC $ do
    dia <- dialogNew
    set dia [windowTitle := "Fold Change Setting"
            ,windowDefaultWidth := 300 
            ,containerBorderWidth := 30 ]
      
    dVBox <- dialogGetUpper dia
    cutOffBox <- hBoxNew False 10

    fcCutOff <- hScaleNew fcAdj1
    tLabel <- labelNew (Just "Fold Change cut-off：")
    boxPackStart cutOffBox tLabel PackNatural 0
    boxPackStart cutOffBox fcCutOff PackGrow 0
    (comBox,refSs) <- compareBox ["sample1","sample2"]
    dialogAddButton dia stockApply  ResponseApply
    dialogAddButton dia stockCancel ResponseCancel    
    containerAdd dVBox cutOffBox
    containerAdd dVBox comBox
    widgetShowAll dVBox
    answer <- dialogRun dia
    if answer == ResponseApply 
      then do
        fc <- adjustmentGetValue fcAdj1
        ss <- readIORef refSs
        when (not $ null ss) $ do
          writeIORef refFSet1 (Just $ F (C fc Nothing) ss)
          buttonSetLabel bFC "设置完毕"
          buttonSetUseStock bFC True
        when (null ss) $ do 
          writeIORef refFSet1 Nothing
          buttonSetLabel bFC "_Fold Change Filtering"
          buttonSetUseUnderline bFC True
          
        widgetDestroy dia
      else widgetDestroy dia

  onClicked bVF $ do
    dia <- dialogNew
    set dia [windowTitle := "Volcano Plot Setting"
            ,windowDefaultWidth := 300 
            ,containerBorderWidth := 30 ]
      
    dVBox <- dialogGetUpper dia
    fcCutOffBox <- hBoxNew False 10
    pCutOffBox <- hBoxNew False 10    

    fcCutOff <- hScaleNew fcAdj2
    pCutOff <- hScaleNew pAdj
    scaleSetDigits pCutOff 3
    tLabel <- labelNew (Just "Fold Change cut-off：")
    pLabel <- labelNew (Just "P-value cut-off：")
    boxPackStart fcCutOffBox tLabel PackNatural 0
    boxPackStart fcCutOffBox fcCutOff PackGrow 0
    boxPackStart pCutOffBox pLabel PackNatural 0
    boxPackStart pCutOffBox pCutOff PackGrow 0
    
    (comBox,refGs) <- compareBox ["group1","group2"]
    dialogAddButton dia stockApply  ResponseApply
    dialogAddButton dia stockCancel ResponseCancel    
    containerAdd dVBox fcCutOffBox
    containerAdd dVBox pCutOffBox
    containerAdd dVBox comBox
    
    widgetShowAll dVBox
    answer <- dialogRun dia
    if answer == ResponseApply 
      then do
        fc <- adjustmentGetValue fcAdj2
        p <- adjustmentGetValue pAdj
        gs <- readIORef refGs
        if null gs
          then do
            writeIORef refFSet2 Nothing
            buttonSetLabel bVF "_Volcano Plot Filtering"
            buttonSetUseUnderline bVF True
            widgetDestroy dia            
          else do
          
            writeIORef refFSet2 $ Just $
              F (C fc (Just (Unpaired,p))) gs
            buttonSetLabel bVF "设置完毕"
            buttonSetUseStock bVF True
              
            widgetDestroy dia
      else widgetDestroy dia
    
    return ()
  containerAdd fcBox bFC
  containerAdd fcBox bVF
  return fcBox

compareBox ls = do
  refPs <- newIORef []
  t <- tableNew 2 4 False
  leftC <- createCBox ls
  rightC <- createCBox ls
  l <- labelNew (Just "vs")
  add <- buttonNewWithMnemonic "_Add"
  clear <- buttonNewWithMnemonic "_Clear"
  scrWin <- scrolledWindowNew Nothing Nothing
  pairShow <- labelNew Nothing
  scrolledWindowAddWithViewport scrWin pairShow
  tableAttachDefaults t leftC 0 1 0 1
  tableAttachDefaults t l 1 2 0 1
  tableAttachDefaults t rightC 2 3 0 1
  tableAttachDefaults t add 3 4 0 1
  tableAttachDefaults t clear 3 4 1 2
  tableAttachDefaults t scrWin 0 3 1 2
  onClicked add $ do
    str1 <- comboBoxGetActiveText leftC
    str2 <- comboBoxGetActiveText rightC
    case (,) <$> str1 <*> str2 of
      Just tp ->
        do
          ps <- readIORef refPs
          let ps' = nub $ ps ++ [tp]
          labelSetText pairShow $ toStr ps'
          writeIORef refPs ps'
            
      Nothing -> return ()
  onClicked clear $ do
    writeIORef refPs []
    labelSetText pairShow ""
  return (t,refPs)

createCBox :: [String] -> IO ComboBox
createCBox ss = do
  cb <- comboBoxNewText
  mapM_ (comboBoxAppendText cb) ss
  return cb

createCBox' :: [String] -> IO ComboBox
createCBox' ss = do
  cb <- comboBoxNewText
  mapM_ (comboBoxAppendText cb) ss
  comboBoxSetActive cb 0
  return cb

toStr ps =
  unlines $ map (\(a,b) -> a ++ " vs " ++ b) ps
