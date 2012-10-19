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

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window [windowTitle := "Expression Profiling Report Maker",
              windowDefaultWidth := 300, 
              containerBorderWidth := 30 ]
  
  topVBox <- vBoxNew False 20
  pBox <- projectBox
  cBox <- chipBox
  fBox <- filterBox
  containerAdd window topVBox
  containerAdd topVBox pBox
  containerAdd topVBox cBox
  containerAdd topVBox fBox
  

  widgetShowAll window 
  
  onDestroy window mainQuit
  mainGUI  
