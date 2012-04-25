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
module PGFPlotsTemplate 
       (
         texTemplate
       , survivalCurveTemplate
       )
       where

import Text.StringTemplate

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
   \\\\\begin{axis}[legend pos=south west,\
                   \legend style={draw=none},\
                   \ymin=0,xmin=0,]\n\
   \$curves;separator='\n'$\n\
   \\\\\end{axis}\n\
   \\\\\end{tikzpicture}\n\
   \\\\\end{document}\n"

survivalCurveTemplate :: StringTemplate String
survivalCurveTemplate = newSTMP
  "\\\\addplot+[mark=+,only marks,forget plot,$color$] coordinates{\n\
  \$censoredPoints$\n\
  \};\n\
  \\\\\addplot+[const plot,mark=none,$color$] coordinates{\n\
  \$coordinates$\n\
  \};\\\\addlegendentry{$legend$}\n"

                
