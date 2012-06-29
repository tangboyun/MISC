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
module GSEATemplate 
       (
         toTexString
       , toTexString'
       )
       where

import Text.StringTemplate
import qualified Data.Vector.Unboxed as UV
import Data.List
type FloatType = Double

texTemplate :: StringTemplate String
texTemplate = newSTMP
  "\\\\documentclass{standalone}\n\
   \\\\\usepackage{tikz}\n\
   \\\\\usetikzlibrary{fadings,mindmap,shadows,shapes.arrows,shapes.geometric,\
                    \shapes.misc,matrix,arrows,positioning,calc,decorations.pathreplacing,\
                    \plotmarks}\n\
   \\\\\usepackage{graphicx}\n\
   \\\\\usepackage{xcolor}\n\
   \\\\\usepackage{times}\n\
   \\\\\usepackage{pgfplots}\n\
   \\\\\begin{document}\n\
   \\\\\begin{tikzpicture}\n\
   \\\\\begin{axis}[\n\
   \axis lines=center,\n\
   \ymax=$yMax$,\n\
   \ymin=$yMin$,\n\
   \xmax=$xMax$,\n\
   \xmin=0,\n\
   \clip=false,\n\
   \enlargelimits=false,\n\
   \xticklabels={,,},\n\
   \scaled ticks=false,\n\
   \legend style={draw=none,},\n\
   \title={$picTitle$},\n\
   \]\n\
   \$curves;separator='\n'$\n\
   \\\\\end{axis}\n\
   \\\\\end{tikzpicture}\n\
   \\\\\end{document}\n"

curveTemplate :: StringTemplate String
curveTemplate = newSTMP
  "\\\\addplot[fill=gray!30,draw=gray!30,opacity=0.5] coordinates{\n\
   \$upperPoints$\n\
   \$lowerPoints$\n\
   \} --cycle;\n\
   \\\\\addlegendentry[scale=0.7]{5-95 Percentile}\n\
   \\\\\addplot[no marks] coordinates{\n\
   \$esPoints$\n\
   \};\n\
   \\\\\addlegendentry[scale=0.7]{Observed}\n\
   \\\\\fill[left color =red!80,right color=blue!80,middle color=white] (axis cs:0,$barMin$) rectangle (axis cs:$idxMax$,$barMax$);\n\
   \\\\\foreach \\\\x in {$geneSetIdx$} {\n\
   \\\\\edef\\\\temp{\\\\noexpand\\\\draw[ultra thin] (axis cs: \\\\x,$barMin$) -- (axis cs: \\\\x,$barMax$);}\n\
   \\\\\temp\n\
   \}\n"


-- | without ci
curveTemplate' :: StringTemplate String
curveTemplate' = newSTMP
  "\\\\addplot[no marks,green] coordinates{\n\
   \$esPoints$\n\
   \};\n\
   \\\\\fill[left color =red!80,right color=blue!80,middle color=white] (axis cs:0,$barMin$) rectangle (axis cs:$idxMax$,$barMax$);\n\
   \\\\\foreach \\\\x in {$geneSetIdx$} {\n\
   \\\\\edef\\\\temp{\\\\noexpand\\\\draw[ultra thin] (axis cs: \\\\x,$barMin$) -- (axis cs: \\\\x,$barMax$);}\n\
   \\\\\temp\n\
   \}\n"

toTexString :: String -> UV.Vector Int -> (UV.Vector FloatType,UV.Vector (FloatType,FloatType)) -> String
toTexString title geneSetIdx (eScore,bounds) = 
  let es = UV.toList $ UV.indexed eScore
      (low,up) = UV.unzip bounds
      ls = reverse $ UV.toList $ UV.indexed low
      us = UV.toList $ UV.indexed up
      barMin = barMax - 0.05 * (yMax-yMin)
      barMax = yMin - 0.02 * (yMax - yMin)
      idxMax = UV.length eScore - 1
      vMin = minimum $ UV.toList $ eScore UV.++ low UV.++ up
      vMax = maximum $ UV.toList $ eScore UV.++ low UV.++ up
      vABS = vMax - vMin
      yMax = vMax + 0.1 * vABS
      yMin = vMin - 0.1 * vABS
      xMax =  floor $ 1.1 * fromIntegral idxMax
      curves = render $ 
               setManyAttrib 
               [("lowerPoints",intercalate "\n" $ map show ls)
               ,("upperPoints",intercalate "\n" $ map show us)
               ,("esPoints",intercalate "\n" $ map show es)
               ,("barMin",show barMin)
               ,("barMax",show barMax)                
               ,("idxMax",show idxMax)
               ,("geneSetIdx",intercalate "," $ map show $ sort $ UV.toList geneSetIdx)
               ] curveTemplate
  in render $ setManyAttrib
     [("curves", curves)
     ,("title", title)
     ,("yMax", show yMax)
     ,("yMin", show yMin)
     ,("xMax", show xMax)
     ,("idxMax", show idxMax)
     ] texTemplate

-- | without ci
toTexString' :: String -> UV.Vector Int -> UV.Vector FloatType -> String
toTexString' title geneSetIdx eScore = 
  let es = UV.toList $ UV.indexed eScore
      barMin = barMax - 0.05 * (yMax-yMin)
      barMax = yMin - 0.02 * (yMax - yMin)
      idxMax = UV.length eScore - 1
      vMin = minimum $ UV.toList $ eScore 
      vMax = maximum $ UV.toList $ eScore 
      vABS = vMax - vMin
      yMax = vMax + 0.1 * vABS
      yMin = vMin - 0.1 * vABS
      xMax =  floor $ 1.1 * fromIntegral idxMax
      curves = render $ 
               setManyAttrib 
               [("esPoints",intercalate "\n" $ map show es)
               ,("barMin",show barMin)
               ,("barMax",show barMax)                
               ,("idxMax",show idxMax)
               ,("geneSetIdx",intercalate "," $ map show $ sort $ UV.toList geneSetIdx)
               ] curveTemplate'
  in render $ setManyAttrib
     [("curves", curves)
     ,("title", title)
     ,("yMax", show yMax)
     ,("yMin", show yMin)
     ,("xMax", show xMax)
     ,("idxMax", show idxMax)
     ] texTemplate


