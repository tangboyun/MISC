{-# LANGUAGE BangPatterns #-}
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

import           Control.Arrow
import           Data.Clustering.Hierarchical
import           Data.List
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Text.StringTemplate

drawMatrixWithLabel :: Double -> Double -> String -> String -> String
                    -> V.Vector (UV.Vector Double) -> (V.Vector String,String) -> (V.Vector String,String) -> Double -> Double -> String
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
  \\\\\draw ($(\\\\x,\\\\y) + (0,<halfLineLen>) + (<angle>:0)$) -- ++(<angle>:<lineLen>) \
  \node [right,rotate=<angle>,font=\\\\<colNameSize>] {\\\\z};\n\
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
   \$dendro$\n\
   \\\\\end{tikzpicture}\n\
   \\\\\end{document}\n"            



data Axis = X
          | Y
          deriving (Eq)
data Direct = Pos
            | Neg
            deriving (Eq)
-- | Borrowed from Diagrams.Dendrogram
fixedWidth :: Double -> Dendrogram a -> (Dendrogram (a, Double), Double)
fixedWidth w = second (subtract half_w) . go half_w
    where
      half_w = w/2
      go !y (Leaf datum) = (Leaf (datum, y), y + w)
      go !y (Branch d l r) = (Branch d l' r', y'')
          where
            (l', !y')  = go y  l
            (r', !y'') = go y' r

deepth :: Dendrogram a -> Double
deepth dendro = deepth' 0 dendro
  where
    deepth' acc (Leaf _) = acc
    deepth' acc (Branch d l r) = d + max (deepth l) (deepth r)


transDendro :: Dendrogram a -> Dendrogram a
transDendro l@(Leaf _) = l
transDendro (Branch d' l r) = Branch (d' + max (deepth l) (deepth r)) (transDendro l) (transDendro r)

drawDendro :: Ord a => (Axis,Direct) -> (Double,Double) -> Double -> Double -> Dendrogram a -> String
drawDendro (axis,direct) (x,y) w scale dendro = 
  let dendro' = fst $ fixedWidth w dendro
  in go dendro'
  where
    mean ls = let (sum,l) = foldl' (\(acc,len) e -> (acc+e,len+1)) (0,0) ls
              in sum / l
    go (Leaf _) = ""
    go (Branch d l r) =
      let wL = mean $ map snd $ elements l
          hL = scale * deepth l
          wR = mean $ map snd $ elements r
          hR = scale * deepth r
          d' = scale * d
      in case axis of
        X ->
          let coordL = (x+wL,y+hL)
              coordL' = if hL == 0 -- leaf
                        then (x+wL,y+hL+d'+hR)
                        else (x+wL,y+hL+d')
              coordR = (x+wR,y+hR)
              coordR' = if hR == 0 -- leaf
                        then (x+wR,y+hR+d'+hL)
                        else (x+wR,y+hR+d')
              mapY = if direct == Pos
                     then id
                     else negY
              negY (x',y') = (x',-y')
          in "\\draw " ++ show (mapY coordL) ++ " -- " ++ show (mapY coordL') ++
             " -- " ++ show (mapY coordR') ++ " -- " ++ show (mapY coordR) ++ ";\n" ++
             go l ++ go r
        Y ->
          let coordL = (x+hL,y+wL)
              coordL' = if hL == 0 -- leaf
                        then (x+hL+d'+hR,y+wL)
                        else (x+hL+d',y+wL)
              coordR = (x+hR,y+wR)
              coordR' = if hR == 0 -- leaf
                        then (x+hR+d'+hL,y+wR)
                        else (x+hR+d',y+wR)
              mapX = if direct == Pos
                     then id
                     else negX
              negX (x',y') = (-x',y')
              
          in "\\draw " ++ show (mapX coordL) ++ " -- " ++ show (mapX coordL') ++
             " -- " ++ show (mapX coordR') ++ " -- " ++ show (mapX coordR) ++ ";\n" ++
             go l ++ go r

  
testDendro = Branch 0.8
             (Branch 0.5
              (Branch 0.2
               (Leaf 'A')
               (Leaf 'B'))
              (Leaf 'C'))
             (Leaf 'D')
