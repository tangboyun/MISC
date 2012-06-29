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


drawMatrixWithLabel :: Double -> Double -> String -> String -> String -> V.Vector (UV.Vector Double) -> (V.Vector String,String) -> (V.Vector String,String) -> Double -> Double -> String
drawMatrixWithLabel width height posColor negColor bgColor m (rowNames,rowNameSize) (colNames,colNameSize) lineLen angle =
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
      rowNameCoor = map (\(l,(x,y)) -> show x ++ "/" ++ show y ++ "/" ++ l) $
                    zip (V.toList rowNames) $
                    map (\idx -> (width, negate $ (fromIntegral idx + 0.5) * h)) [0..nRow-1]
      colNameCoor = map (\(l,(x,y)) -> show x ++ "/" ++ show y ++ "/" ++ l) $
                    zip (V.toList colNames) $
                    map (\idx -> ((fromIntegral idx + 0.5) * w,0)) [0..nCol-1]
      label = render $ setManyAttrib
              [("rowTuples",rowNameCoor)
              ,("colTuples",colNameCoor)
              ,("lineLen", [show lineLen])
              ,("halfLineLen",[show (lineLen/2)])
              ,("angle",[show angle])
              ,("colNameSize",[colNameSize])
              ,("rowNameSize",[rowNameSize])
              ] labelTemplate
  in render $ setManyAttrib
              [("rectangles",rects)
              ,("labels",[label])
              ] texTemplate


labelTemplate :: StringTemplate String
labelTemplate = newAngleSTMP
  "\\\\foreach \\\\x/\\\\y/\\\\z in {<rowTuples;separator=','>}{\n\
  \\\\\draw (\\\\x,\\\\y) --++(<halfLineLen>,0) node [right,font=\\\\<rowNameSize>] {\\\\z};\n\
  \}\n\
  \\\\\foreach \\\\x/\\\\y/\\\\z in {<colTuples;separator=','>}{\n\
  \\\\\draw[] (\\\\x,\\\\y) -- ++(0,<halfLineLen>);\n\
  \\\\\draw ($(\\\\x,\\\\y) + (0,<halfLineLen>) + (<angle>:0)$) -- ++(<angle>:<lineLen>) node [right,rotate=<angle>,font=\\\\<colNameSize>] {\\\\z};\n\
  \}\n"


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
   \\\\\usetikzlibrary{calc}\n\
   \\\\\begin{document}\n\
   \\\\\begin{tikzpicture}\n\
   \$rectangles;separator='\n'$\n\
   \$labels$\n\
   \\\\\end{tikzpicture}\n\
   \\\\\end{document}\n"            
