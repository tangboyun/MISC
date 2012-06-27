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

module Draw where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import Text.StringTemplate

drawMatrix :: Double -> Double -> String -> String -> String -> V.Vector (UV.Vector Double) -> String
drawMatrix width height posColor negColor bgColor m =
  let nRow = V.length m
      nCol = UV.length $ m `V.unsafeIndex` 0
      w = width / fromIntegral nCol
      h = height / fromIntegral nRow
      rects = concatMap
              (\(i,row) ->
                map
                (\(j,v) ->
                  rectangle posColor negColor bgColor w h v (w * fromIntegral j,-h * fromIntegral i)) $
                UV.toList $ UV.indexed row) $
              V.toList $ V.indexed m
  in render $ setAttribute "rectangles" rects texTemplate

rectangle :: String -> String -> String -> Double -> Double -> Double -> (Double,Double) -> String
rectangle posColor negColor bgColor width height value coor@(x,y) =
  let color = if value > 0
              then posColor
              else negColor
  in render $ setManyAttrib
     [("color",color)
     ,("bgColor",bgColor)
     ,("value", show $! abs $ round $ value * 100)
     ,("lu", show coor)
     ,("ru", show (x+width,y))
     ,("rd", show (x+width,y-height))
     ,("ld", show (x,y-height))
     ] rectangleTemplate
  
  
rectangleTemplate :: StringTemplate String
rectangleTemplate = newSTMP
  "\\\\fill[$color$!$value$!$bgColor$] $lu$ -- $ru$ -- $rd$ -- $ld$ --cycle;"

texTemplate :: StringTemplate String
texTemplate = newSTMP
  "\\\\documentclass{standalone}\n\
   \\\\\usepackage{graphicx}\n\
   \\\\\usepackage{tikz}\n\
   \\\\\begin{document}\n\
   \\\\\begin{tikzpicture}\n\
   \$rectangles;separator='\n'$\n\
   \\\\\end{tikzpicture}\n\
   \\\\\end{document}\n"            
