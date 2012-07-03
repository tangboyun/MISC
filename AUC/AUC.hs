-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference : Tom Fawcett, An introduction to ROC analysis, Pattern Recognition Letters,
--             Volume 27, Issue 8, June 2006, Pages 861-874
-- 
--
-----------------------------------------------------------------------------

module AUC where

import qualified Data.Vector.Unboxed as UV
import Data.List
import Data.Function
import Text.StringTemplate
import Debug.Trace
type FloatType = Double

toROC :: UV.Vector Bool -> UV.Vector FloatType -> [(FloatType,FloatType)]
toROC label scores =
  let ordIdx = UV.fromList $
               map fst $
               sortBy (flip compare `on` snd) $
               UV.toList $ UV.indexed scores
      n = UV.length label
      labelOrd = UV.unsafeBackpermute label ordIdx
      scoreOrd = UV.unsafeBackpermute scores ordIdx
      np = UV.length $ UV.findIndices (id) labelOrd
      nn = n - np
      (ps,_,_) = foldl'
                 (\(acc,(tp,fp),s) idx ->
                   let (tp',fp') = if labelOrd `UV.unsafeIndex` idx
                                   then (tp+1,fp)
                                   else (tp,fp+1)
                       s' = scoreOrd `UV.unsafeIndex` idx
                       acc' = if s /= (scoreOrd `UV.unsafeIndex` idx)
                              then (fp / fromIntegral nn,tp / fromIntegral np) : acc 
                              else acc
                   in (acc',(tp',fp'),s')
                 ) ([],(0.0,0.0),negate 1.0/0) [0..n-1]
  in (1,1):ps

auc :: UV.Vector Bool -> UV.Vector FloatType -> FloatType
auc label scores =
  let ordIdx = UV.fromList $
               map fst $
               sortBy (flip compare `on` snd) $
               UV.toList $ UV.indexed scores
      n = UV.length label
      labelOrd = UV.unsafeBackpermute label ordIdx
      scoreOrd = UV.unsafeBackpermute scores ordIdx
      np = UV.length $ UV.findIndices (id) labelOrd
      nn = n - np
      (a,(tpl,fpl),_) = foldl'
            (\(area,(tp,fp),s) idx ->
              let (tp',fp') = if labelOrd `UV.unsafeIndex` idx
                              then (tp+1,fp)
                              else (tp,fp+1)
                  s' = scoreOrd `UV.unsafeIndex` idx
                  area' = if s /= (scoreOrd `UV.unsafeIndex` idx)
                          then area + (fp' - fp) * 0.5 * (tp' + tp)
                          else area
              in (area',(tp',fp'),s')
            ) (0,(0,0),negate 1.0/0) [0..n-1]
  in a / fromIntegral (np * nn)

drawROC :: [(String,UV.Vector (Bool,FloatType))] -> String
drawROC labels =
  let curves = map
               (\(label,vec) ->
                 let (lV,fV) = UV.unzip vec
                 in render $ setManyAttrib
                    [("points" , map show $ toROC lV fV)
                    ,("legend" , [label])
                    ] rocCurveTemplate
               ) labels
  in render $ setAttribute "curves" curves texTemplate

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
   \\\\\begin{axis}[legend pos=south east,\
                   \legend style={draw=none},\
                   \xlabel={False positive rate},\
                   \ylabel={True positive rate},\
                   \ymin=0,xmin=0,ymax=1,xmax=1]\n\
   \$curves;separator='\n'$\n\
   \\\\\end{axis}\n\
   \\\\\end{tikzpicture}\n\
   \\\\\end{document}\n"

rocCurveTemplate :: StringTemplate String
rocCurveTemplate = newSTMP
  "\\\\addplot+[no marks,$color$] coordinates{\n\
  \$points;separator='\n'$\n\
  \};\n\
  \\\\\addlegendentry{$legend$}\n"

-- | no legend
rocCurveTemplate' :: StringTemplate String
rocCurveTemplate' = newSTMP
  "\\\\addplot+[no marks,$color$] coordinates{\n\
  \$points;separator='\n'$\n\
  \};"
  
-- | dataset from the reference (Fig.3)
testData :: UV.Vector (Bool,FloatType)
testData = UV.fromList [(True,0.9)
                       ,(True,0.8)
                       ,(False,0.7)
                       ,(True,0.6)
                       ,(True,0.55)
                       ,(True,0.54)
                       ,(False,0.53)
                       ,(False,0.52)
                       ,(True,0.51)
                       ,(False,0.505)
                       ,(True,0.4)
                       ,(False,0.39)
                       ,(True,0.38)
                       ,(False,0.37)
                       ,(False,0.36)
                       ,(False,0.35)
                       ,(True,0.34)
                       ,(False,0.33)
                       ,(True,0.3)
                       ,(False,0.1)]
