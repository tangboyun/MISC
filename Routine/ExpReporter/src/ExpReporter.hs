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

import Data.Version
import Graphics.UI.Gtk
import Report.GUI.Layout

version = Version [0,1,0,17] []

main :: IO ()
main = do
  initGUI
  window <- mkGUI
  set window [windowTitle := "Expression Profiling Report Maker" ++
                             " " ++ "v" ++ showVersion version
             ,windowDefaultWidth := 300 
             ,containerBorderWidth := 30
             ]
  
  widgetShowAll window 
  onDestroy window mainQuit

  mainGUI  


