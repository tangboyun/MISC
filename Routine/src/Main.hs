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

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import GUI
import Data.IORef

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
  iBox <- inputBox
  oBox <- outputBox
  
  aBox <- hBoxNew False 0
  rB <- buttonNewFromStock stockApply
  boxPackStart aBox rB PackRepel 0
    
  
  tBox <- fcBox refFC refVP
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
    print fcSetting
    print vpSetting
    print chip
    print rna
    print sp

  widgetShowAll window 
  
  onDestroy window mainQuit
  mainGUI  
