-----------------------------------------------------------------------------
-- |
-- Module : 函数绘制
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------
module Function
       (
         plotFun
       , pathAlongFun
       )
       where

import Diagrams.Attributes
import Diagrams.TwoD
import Diagrams.Path

-- | 函数绘制。原点位于起始点
-- >>> plotFun 颜色 样本点数 (起始，终止) 函数
plotFun c n (x1,x2) f = lineColor c $ stroke $
                        pathAlongFun n (x1,x2) f

-- | 返回函数的路径。原点位于起始点
-- >>> pathAlongFun 样本点数 (起始，终止) 函数
pathAlongFun n (x1,x2) f =
  let ps = [x1,(x2 - x1) / fromIntegral n .. x2]
  in fromVertices $ map p2 $ zip ps $ map f ps
     
