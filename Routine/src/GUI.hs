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

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

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
  cb <- hBoxNew True 10
  chipTypeBox <- hBoxNew False 0
  chipTypeLabel <- labelNew (Just "芯片类型：")
  chipType <- createCBox ["表达谱芯片", "LncRNA芯片"]
  boxPackStart chipTypeBox chipTypeLabel PackNatural 0
  boxPackStart chipTypeBox chipType PackNatural 0

  chipSpBox <- hBoxNew False 0
  chipSpLabel <- labelNew (Just "物种：")
  chipSp <- createCBox ["人类", "小鼠", "大鼠"]
  boxPackStart chipSpBox chipSpLabel PackNatural 0
  boxPackStart chipSpBox chipSp PackNatural 0

  chipVendorBox <- hBoxNew False 0
  chipVendorLabel <- labelNew (Just "厂商：")
  chipVendor <- createCBox ["Agilent", "Arraystar"]
  boxPackStart chipVendorBox chipVendorLabel PackNatural 0
  boxPackStart chipVendorBox chipVendor PackNatural 0
  
  boxPackStart cb chipTypeBox PackNatural 0
  boxPackStart cb chipSpBox PackNatural 0
  boxPackStart cb chipVendorBox PackNatural 0
  return cb
  
filterBox = do
  fBox <- hBoxNew False 10
  fcBox <- vBoxNew False 10
  volBox <- vBoxNew False 10
  fcTitle <- labelNew (Just "Fold Change Filtering")
  fcCutOffLabel1 <- labelNew (Just "Fold Change cut-off：")
  fcCutOffLabel2 <- labelNew (Just "Fold Change cut-off：")  
  pCutOffLabel <- labelNew (Just "P-value cut-off：")
  volTitle <- labelNew (Just "Volcano Plot Filtering")
  labelSetJustify fcTitle JustifyLeft
  labelSetJustify volTitle JustifyLeft

  boxPackStart fcBox fcTitle PackRepel 0
  boxPackStart fcBox fcCutOffLabel1 PackGrow 0
  
  boxPackStart volBox volTitle PackRepel 0
  boxPackStart volBox fcCutOffLabel2 PackGrow 0
  
  boxPackStart fBox fcBox PackNatural 0
  boxPackStart fBox volBox PackNatural 0
  return fBox
  
createCBox :: [String] -> IO ComboBox
createCBox ss = do
  cb <- comboBoxNewText
  mapM_ (comboBoxAppendText cb) ss
  return cb
