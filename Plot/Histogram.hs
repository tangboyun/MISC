{-# LANGUAGE NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
-- |
-- Module : 一些柱状图相关便利函数
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------
module Histogram where

import Diagrams.Attributes
import Diagrams.TwoD


-- | 柱状图绘制。给定颜色与宽度，绘制一系列柱形。原点为第一个柱状体底部中点。
-- >>> his 颜色 宽度 [高度]
his c w = hcat . map (fillColor c . alignB . rect w)

