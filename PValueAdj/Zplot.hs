{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference : Microarrays, Empirical Bayes, and the Two-Groups Model
--             Bradley Efron
-- 
--
-----------------------------------------------------------------------------

module ZPlot where

import           Control.Monad
import           Control.Monad.ST.Strict
import           Data.List
import           Data.List.Split
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM
import           Numeric.GSL.Minimization
import           Statistics.Distribution
import           Statistics.Distribution.Normal
import           Statistics.Distribution.StudentT
import           Statistics.Basic
import           Statistics.Sampling
import           System.Random.MWC
import           Text.StringTemplate

type FloatType = Double

-- | 对2组非配对数据做permutation，返回z值分布
permUnpaired :: Seed -> Int -> V.Vector (UV.Vector FloatType) -> UV.Vector Bool -> (V.Vector (UV.Vector FloatType),Seed)
permUnpaired seed nRepeat expData label =
  let (rV,seed') = foldl
                   (\(acc,s) _ ->
                     let (r,s') = shuffle s label
                     in (acc `V.snoc` r,s')
                   ) (V.empty,seed) [1..nRepeat]
  in (V.map (zstat expData) rV,seed')

-- | 配对数据的permutation，返回z值
permPaired :: Seed -> Int -> V.Vector (UV.Vector FloatType) -> (V.Vector (UV.Vector FloatType),Seed)
permPaired seed nRepeat disVec =
  let nSample = V.length disVec
      df = nSample - 1
      n = nRepeat * nSample
      (rs,seed') = runST $ do
        gen <- restore seed
        rs <- replicateM n $ uniformR ((-1,1)::(Int,Int)) gen
        s <- save gen
        return (map ((*2) . (\e -> e - 0.5) . fromIntegral) rs,s)
      permZ = V.fromList $
              map
              (
                (\(m,v) -> UV.map (tToZ df) $!
                          UV.zipWith (/) m $
                          UV.map ((/ (sqrt $ fromIntegral nSample)) . sqrt) v) .
                vecFastMeanVar .
                V.zipWith (\v c -> UV.map (* c) v) disVec .
                V.fromList
              ) $ splitEvery nSample rs
  in (permZ,seed')


-- | plot p-value分布柱状图，返回string可以写入生成一个tex文件
plotP :: Int -> UV.Vector FloatType -> FloatType -> String -> String -> String
plotP nBin pVec opacity color title =
  let !pVec' = UV.fromList $ sort $ UV.toList pVec
      !n = UV.length pVec
      !binLen = fromIntegral n / fromIntegral nBin
      !scaleCoef = 1 / step / fromIntegral n
      !minV = 0
      !maxV = 1
      !step = (maxV - minV) / fromIntegral nBin      
      ls = init $ tail $ [minV,(minV+step)..maxV]
      vs = toHis ls pVec'
      vs' = map (* scaleCoef) vs      
      !yMax = maximum vs'
      y = -0.025 * yMax
      is = zip vs' $
           init $ [minV,(minV+step)..maxV]

      curves = map (\(y,x) ->
                     render $ setManyAttrib [("color",color)
                                            ,("opacity",show opacity)
                                            ,("x1",show x)
                                            ,("x2",show $ x+step)
                                            ,("height",show y)] rectTemplate
                   ) is
      xTicks = map (\xl ->
                     render $ setManyAttrib [("xLabel",show xl)
                                            ,("y",show y)
                                            ,("yDouble", show $! 2*y)] tickTemplate
                     ) [0,0.2,0.4,0.6,0.8,1]
      pPlotTemplate = newSTMP
                      "\\\\documentclass{standalone}\n\
                      \\\\\usepackage{tikz}\n\
                      \\\\\usetikzlibrary{fadings,mindmap,shadows,shapes.arrows,shapes.geometric,\
                      \shapes.misc,matrix,arrows,positioning,calc,decorations.pathreplacing,\
                      \plotmarks}\n\
                      \\\\\usepackage{graphicx}\n\
                      \\\\\usepackage{xcolor}\n\
                      \\\\\usepackage{times}\n\
                      \\\\\usepackage{pgfplots}\n\
                      \\\\\usepackage{amsbsy}\n\
                      \\\\\begin{document}\n\
                      \\\\\begin{tikzpicture}\n\
                      \\\\\begin{axis}[axis lines=none,\n\
                      \title={$title$},\n\
                      \clip=false,\n\
                      \]\n\
                      \$curves;separator='\n'$\n\
                      \\\\\addplot[thick,draw=none] gnuplot [smooth,no marks,domain=0:1] {0};\n\
                      \\\\\draw[thick] (axis cs:0,$y$) -- (axis cs:1,$y$);\n\
                      \$ticks;separator='\n'$\n\
                      \\\\\node (pos) [] at (axis cs:0.5,0) {};\n\
                      \\\\\node[yshift=-2.5em,font=\\\\Large] at (pos) {\\\\(p\\\\)-value};\n\
                      \\\\\end{axis}\n\
                      \\\\\end{tikzpicture}\n\
                      \\\\\end{document}\n"
  in render $ setManyAttrib
     [("curves",curves)
     ,("y",[show y])
     ,("y1",[show $ 2*y])
     ,("ticks",xTicks)
     ,("title",[title])
     ] pPlotTemplate

-- | 生成permutation、BH、Bayes方法的FDR示意图，并包含实际数据的z值分布
plotPermDis' :: Int -> Int -> UV.Vector FloatType -> V.Vector (UV.Vector FloatType)
             -> (FloatType,FloatType) -> FloatType -> (String,String,String,String) -> String -> String
plotPermDis' nBin df trueZ permZ (delta,sigma) opacity (colorP,colorN,colorT,colorE) title =
  let trueZ' = UV.fromList $ sort $ UV.toList trueZ
      !n = UV.length trueZ
      !nPerm = V.length permZ
      !binLen = fromIntegral n / fromIntegral nBin
      !scaleCoef = 1 / step / fromIntegral n
      !minV = UV.head trueZ'
      !maxV = UV.last trueZ'
      !step = (maxV - minV) / fromIntegral nBin
      ls = init $ tail $ [minV,(minV+step)..maxV]
      vs = toHis ls trueZ'
      vs' = map (* scaleCoef) vs      
      (pls,pvs) = runST $ do
        let !permLs = UV.fromList $
                        [minz,minz+step..maxz]
            !minz = minV - fromIntegral nBin * step
            !maxz = maxV + fromIntegral nBin * step
            !len = UV.length permLs
        mv <- UVM.replicate len 0
        V.forM_ permZ $! \zVec -> 
          UV.forM_ zVec $! \z -> do
            let !idx = floor $! (z - minz) / step
            v <- UVM.unsafeRead mv idx
            UVM.unsafeWrite mv idx (v+1)
        acc <- UV.unsafeFreeze mv
        let !idx' = UV.findIndices (/= 0) acc
            !acc' = UV.map (/ fromIntegral nPerm) acc
            !resultls = UV.unsafeBackpermute permLs idx'
            !resultvs = UV.toList $! UV.unsafeBackpermute acc' idx'
        return (resultls,
                resultvs)
      pvs' = map (* scaleCoef) pvs

      !yMax = max (density standard 0) $ max (maximum vs') (maximum pvs')
      y = -0.025 * yMax
      is' = zip pvs' $
            init $ UV.toList pls
      is = zip vs' $
           init $ [minV,(minV+step)..maxV]

      curves = map (\(y,x) ->
                     render $ setManyAttrib [("color",colorP)
                                            ,("opacity",show opacity)
                                            ,("x1",show x)
                                            ,("x2",show $ x+step)
                                            ,("height",show y)] rectTemplate
                   ) is' ++
               map (\(y,x) ->
                     render $ setManyAttrib [("color",colorT)
                                            ,("opacity",show opacity)
                                            ,("x1",show x)
                                            ,("x2",show $ x+step)
                                            ,("height",show y)] rectTemplate
                   ) is
      xMin = min (UV.head pls) minV
      xMax = max (UV.last $ UV.init pls) maxV
      xlabels = filter even [floor xMax,(floor xMax-1)..ceiling xMin]
      xTicks = map (\xl ->
                     render $ setManyAttrib [("xLabel",show xl)
                                            ,("y",show y)
                                            ,("yDouble", show $! 2*y)] tickTemplate
                     ) xlabels
      legend = disLegend' (delta,sigma) (0 + 0.45 * xMax) yMax step opacity (colorT,colorP,colorN,colorE)
      d = normalDistr delta sigma
      eMin = quantile d (1e-4)
      eMax = quantile d (1-1e-4)
  in render $ setManyAttrib
     [("curves",curves)
     ,("xMin",[show xMin])
     ,("xMax",[show xMax])
     ,("eMin",[show eMin])
     ,("eMax",[show eMax])
     ,("delta",[show delta])
     ,("sigma",[show sigma])
     ,("colorEm",[colorE])
     ,("y",[show y])
     ,("y1",[show $ 2*y])
     ,("ticks",xTicks)
     ,("nums",map show xlabels)
     ,("colorNull",[colorN])
     ,("legends",legend)
     ,("df",[show df])
     ,("title",[title])
     ] hisTemplate'

-- | 图标标签
disLegend' (delta,sigma) x height step op (colorT,colorP,colorN,colorE) =  
  let ls = take (length vs) [x,x+step..]
      factor = 0.02
      hs = [2,3.5,4.5,5,4.5,3.5,2]
      vs = map (* (factor * height)) hs
      his = map (\(v,l) ->
                  render $
                  setManyAttrib [("color",colorT)
                                ,("opacity",show op)
                                ,("x1",show l)
                                ,("x2",show $ l+step)
                                ,("y", show $ 0.9 * height)
                                ,("heightY",show $ 0.9 * height + v)] rectTemplate'
                   ) (zip vs ls) ++
            map (\(v,l) ->
                  render $
                  setManyAttrib [("color",colorP)
                                ,("opacity",show op)
                                ,("x1",show l)
                                ,("x2",show $ l+step)
                                ,("y", show $ 0.75 * height)
                                ,("heightY",show $ 0.75 * height + v)] rectTemplate'
                   ) (zip vs ls) ++
            ["\\draw[thick,dashed,"++colorN ++ "] (axis cs:"++ show x ++ "," ++
             show y4' ++ ") -- (axis cs:" ++ show x1' ++ "," ++ show y3' ++ ");\n"] ++
            ["\\draw[thick,"++colorE ++ "] (axis cs:"++ show x ++ "," ++
             show y3' ++ ") -- (axis cs:" ++ show x1' ++ "," ++ show y4' ++ ");\n"]
      x1' = x + (fromIntegral $ length vs) * step
      y1' = 0.9 * height + (maximum hs / 2) * factor * height
      y2' = 0.75 * height + (maximum hs / 2) * factor * height
      y3' = 0.6 * height + (maximum hs / 2) * factor * height
      y4' = 0.45 * height + (maximum hs / 2) * factor * height
      node = "\\node[anchor=west,xshift=0.5em,font=\\scriptsize,text=" ++ colorT ++
             "!70!black] at (axis cs:" ++ show x1' ++ "," ++ show y1' ++ ") {$Z$-value Distribution};\n" ++
             "\\node[anchor=west,xshift=0.5em,font=\\scriptsize,text=" ++ colorP ++
             "!70!black] at (axis cs:" ++ show x1' ++ "," ++ show y2' ++ ") {Permutation $\\hat{H_0}$};\n" ++
             "\\node[anchor=west,xshift=0.5em,font=\\scriptsize,text=" ++ colorE ++
             "!70!black] at (axis cs:" ++ show x1' ++ "," ++ show y3' ++ ") {Empirical Bayes $\\hat{H_0}$: \\(N("++
             show (fromIntegral (round $ delta * 100) * 0.01) ++ "," ++
             show (fromIntegral (round $ sigma * 100) * 0.01) ++ ")\\)};\n" ++
             "\\node[anchor=west,xshift=0.5em,font=\\scriptsize,text=" ++ colorN ++
             "!70!black] at (axis cs:" ++ show x1' ++ "," ++ show y4' ++ ") {Therotical $H_0$: $N(0,1)$};\n" 
  in node : his

hisTemplate' :: StringTemplate String
hisTemplate' = newSTMP
  "\\\\documentclass{standalone}\n\
   \\\\\usepackage{tikz}\n\
   \\\\\usetikzlibrary{fadings,mindmap,shadows,shapes.arrows,shapes.geometric,\
                    \shapes.misc,matrix,arrows,positioning,calc,decorations.pathreplacing,\
                    \plotmarks}\n\
   \\\\\usepackage{graphicx}\n\
   \\\\\usepackage{xcolor}\n\
   \\\\\usepackage{times}\n\
   \\\\\usepackage{pgfplots}\n\
   \\\\\usepackage{amsbsy}\n\
   \\\\\begin{document}\n\
   \\\\\begin{tikzpicture}\n\
   \\\\\begin{axis}[axis lines=none,\n\
   \title={$title$},\n\
   \clip=false,\n\
   \legend style={draw=none},\n\
   \]\n\
   \$curves;separator='\n'$\n\
   \\\\\addplot[thick,dashed,$colorNull$] gnuplot [smooth,no marks,domain=-3.5:3.5] {(1/sqrt(2 * pi)) * exp(-0.5*(x*x))};\n\
   \\\\\addplot[thick,$colorEm$] gnuplot [smooth,no marks,domain=$eMin$:$eMax$] {1/(sqrt(2*pi)*$sigma$) * exp(-0.5*(x-$delta$)*(x-$delta$)/($sigma$*$sigma$))};\n\
   \\\\\draw[thick] (axis cs:$xMin$,$y$) -- (axis cs:$xMax$,$y$);\n\
   \$ticks;separator='\n'$\n\
   \$legends;separator='\n'$\n\
   \\\\\node (pos) [] at (axis cs:0,0) {};\n\
   \\\\\node[yshift=-3em] at (pos) {\\\\(\\\\boldsymbol{z_i=\\\\Phi^{-1}(F_{$df$}(t_i))}\\\\)};\n\
   \\\\\end{axis}\n\
   \\\\\end{tikzpicture}\n\
   \\\\\end{document}\n"

-- | plot BH和Permutation的FDR示意，并包含实际数据的z值分布
plotPermDis :: Int -> Int -> UV.Vector FloatType -> V.Vector (UV.Vector FloatType) -> FloatType -> (String,String,String) -> String -> String
plotPermDis nBin df trueZ permZ opacity (colorP,colorN,colorT) title =
  let trueZ' = UV.fromList $ sort $ UV.toList trueZ
      n = UV.length trueZ
      nPerm = V.length permZ
      binLen = fromIntegral n / fromIntegral nBin
      scaleCoef = 1 / step / fromIntegral n
      minV = UV.head trueZ'
      maxV = UV.last trueZ'
      step = (maxV - minV) / fromIntegral nBin
      ls = init $ tail $ [minV,(minV+step)..maxV]
      vs = toHis ls trueZ'
      vs' = map (* scaleCoef) vs      
      (pls,pvs) = runST $ do
        let permLs = UV.fromList $
                        [minz,minz+step..maxz]
            minz = minV - fromIntegral nBin * step
            maxz = maxV + fromIntegral nBin * step
            len = UV.length permLs
        mv <- UVM.replicate len 0
        V.forM_ permZ $! \zVec -> 
          UV.forM_ zVec $! \z -> do
            let !idx = floor $! (z - minz) / step
            v <- UVM.unsafeRead mv idx
            UVM.unsafeWrite mv idx (v+1)
        acc <- UV.unsafeFreeze mv
        let !idx' = UV.findIndices (/= 0) acc
            !acc' = UV.map (/ fromIntegral nPerm) acc
        return (UV.unsafeBackpermute permLs idx',
                UV.toList $ UV.unsafeBackpermute acc' idx')
      pvs' = map (* scaleCoef) pvs

      yMax = max (density standard 0) $ max (maximum vs') (maximum pvs')
      y = -0.025 * yMax
      is' = zip pvs' $
            init $ UV.toList pls
      is = zip vs' $
           init $ [minV,(minV+step)..maxV]

      curves = map (\(y,x) ->
                     render $ setManyAttrib [("color",colorP)
                                            ,("opacity",show opacity)
                                            ,("x1",show x)
                                            ,("x2",show $ x+step)
                                            ,("height",show y)] rectTemplate
                   ) is' ++
               map (\(y,x) ->
                     render $ setManyAttrib [("color",colorT)
                                            ,("opacity",show opacity)
                                            ,("x1",show x)
                                            ,("x2",show $ x+step)
                                            ,("height",show y)] rectTemplate
                   ) is
      xMin = min (UV.head pls) minV
      xMax = max (UV.last $ UV.init pls) maxV
      xlabels = filter even [floor xMax,(floor xMax-1)..ceiling xMin]
      xTicks = map (\xl ->
                     render $ setManyAttrib [("xLabel",show xl)
                                            ,("y",show y)
                                            ,("yDouble", show $! 2*y)] tickTemplate
                     ) xlabels
      legend = disLegend (0 + 0.45 * xMax) yMax step opacity (colorT,colorP,colorN)
  in render $ setManyAttrib
     [("curves",curves)
     ,("xMin",[show xMin])
     ,("xMax",[show xMax])
     ,("y",[show y])
     ,("y1",[show $ 2*y])
     ,("ticks",xTicks)
     ,("nums",map show xlabels)
     ,("colorNull",[colorN])
     ,("legends",legend)
     ,("df",[show df])
     ,("title",[title])
     ] hisTemplate



hisTemplate :: StringTemplate String
hisTemplate = newSTMP
  "\\\\documentclass{standalone}\n\
   \\\\\usepackage{tikz}\n\
   \\\\\usetikzlibrary{fadings,mindmap,shadows,shapes.arrows,shapes.geometric,\
                    \shapes.misc,matrix,arrows,positioning,calc,decorations.pathreplacing,\
                    \plotmarks}\n\
   \\\\\usepackage{graphicx}\n\
   \\\\\usepackage{xcolor}\n\
   \\\\\usepackage{times}\n\
   \\\\\usepackage{pgfplots}\n\
   \\\\\usepackage{amsbsy}\n\
   \\\\\begin{document}\n\
   \\\\\begin{tikzpicture}\n\
   \\\\\begin{axis}[axis lines=none,\n\
   \title={$title$},\n\
   \clip=false,\n\
   \legend style={draw=none},\n\
   \]\n\
   \$curves;separator='\n'$\n\
   \\\\\addplot[thick,dashed,$colorNull$] gnuplot [smooth,no marks,domain=-3.5:3.5] {(1/sqrt(2 * pi)) * exp(-0.5*(x*x))};\n\
   \\\\\draw[thick] (axis cs:$xMin$,$y$) -- (axis cs:$xMax$,$y$);\n\
   \$ticks;separator='\n'$\n\
   \$legends;separator='\n'$\n\
   \\\\\node (pos) [] at (axis cs:0,0) {};\n\
   \\\\\node[yshift=-3em] at (pos) {\\\\(\\\\boldsymbol{z_i=\\\\Phi^{-1}(F_{$df$}(t_i))}\\\\)};\n\
   \\\\\end{axis}\n\
   \\\\\end{tikzpicture}\n\
   \\\\\end{document}\n"

disLegend x height step op (colorT,colorP,colorN) =  
  let ls = take (length vs) [x,x+step..]
      factor = 0.02
      hs = [2,3.5,4.5,5,4.5,3.5,2]
      vs = map (* (factor * height)) hs
      his = map (\(v,l) ->
                  render $
                  setManyAttrib [("color",colorT)
                                ,("opacity",show op)
                                ,("x1",show l)
                                ,("x2",show $ l+step)
                                ,("y", show $ 0.9 * height)
                                ,("heightY",show $ 0.9 * height + v)] rectTemplate'
                   ) (zip vs ls) ++
            map (\(v,l) ->
                  render $
                  setManyAttrib [("color",colorP)
                                ,("opacity",show op)
                                ,("x1",show l)
                                ,("x2",show $ l+step)
                                ,("y", show $ 0.75 * height)
                                ,("heightY",show $ 0.75 * height + v)] rectTemplate'
                   ) (zip vs ls) ++
            ["\\draw[thick,dashed,"++colorN ++ "] (axis cs:"++ show x ++ "," ++
             show y3' ++ ") -- (axis cs:" ++ show x1' ++ "," ++ show y3' ++ ");\n"]
      x1' = x + (fromIntegral $ length vs) * step
      y1' = 0.9 * height + (maximum hs / 2) * factor * height
      y2' = 0.75 * height + (maximum hs / 2) * factor * height
      y3' = 0.6 * height + (maximum hs / 2) * factor * height 
      node = "\\node[anchor=west,xshift=0.5em,font=\\scriptsize,text=" ++ colorT ++
             "!70!black] at (axis cs:" ++ show x1' ++ "," ++ show y1' ++ ") {$Z$-value Distribution};\n" ++
             "\\node[anchor=west,xshift=0.5em,font=\\scriptsize,text=" ++ colorP ++
             "!70!black] at (axis cs:" ++ show x1' ++ "," ++ show y2' ++ ") {Permutation $\\hat{H_0}$};\n" ++
             "\\node[anchor=west,xshift=0.5em,font=\\scriptsize,text=" ++ colorN ++
             "!70!black] at (axis cs:" ++ show x1' ++ "," ++ show y3' ++ ") {Therotical $H_0$: $N(0,1)$};\n"             
  in node : his

   
tickTemplate = newSTMP "\\\\draw[very thin] (axis cs:$xLabel$,$y$) -- (axis cs:$xLabel$,$yDouble$) node [below] {\\\\($xLabel$\\\\)};"
rectTemplate = newSTMP "\\\\draw[fill=$color$,opacity=$opacity$] (axis cs:$x1$,0) rectangle (axis cs:$x2$,$height$);"
rectTemplate' = newSTMP "\\\\draw[fill=$color$,opacity=$opacity$] (axis cs:$x1$,$y$) rectangle (axis cs:$x2$,$heightY$);"
xLabelTemplate = newSTMP "\\\\draw[very thin] (axis cs:$xLabel$,$y$) -- (axis cs:$xLabel$,$yDouble$);"
yLabelTemplate = newSTMP "\\\\draw[very thin] (axis cs:$xMin$,$y$) -- (axis cs:$xNegate$,$y$);"
xTickTemplate = newAngleSTMP "\\\\node[font=\\\\scriptsize] at (axis cs:<xLabel>,<yTri>) {$<xLabel>$};"
yTickTemplate = newAngleSTMP "\\\\node[font=\\\\scriptsize,anchor=east] at (axis cs:<xNegate>,<yCoor>) {$<yLabel>$};"

xAxisTemplate :: StringTemplate String
xAxisTemplate = newSTMP
  "\\\\draw[thick,yshift=-0.3em] (axis cs:$xMin$,0) -- (axis cs:$xMax$,0);\n\
  \\\\\node (point1) [yshift=-0.3em] at (axis cs:0,0) {};\n\
  \\\\\node (point2) [yshift=-0.5em] at (axis cs:0,0) {};\n\
  \\\\\foreach \\\\x in {$nums$} {\n\
  \\\\\edef\\\\temp{\\\\noexpand\\\\draw[ultra thin] let \\\\p1=(point1),\\\\p2=(point2) in \
  \(axis cs: \\\\x,\\\\y1) -- (axis cs: \\\\x,\\\\y2) node [font=\\\\scriptsize,below] {\\\\(\\\\x\\\\)} ;}\n\
  \\\\\temp\n\
  \}\n"



-- | 生成实际z值分布、Bayes与BH FDR的示意图，可显示频次
hist :: Int -> Int -> UV.Vector FloatType -> (FloatType,FloatType) -> FloatType -> String -> String -> String
hist nBin df vec (delta,sigma) opacity color title =
  let vec' = UV.fromList $ sort $ UV.toList vec
      n = UV.length vec
      binLen = fromIntegral n / fromIntegral nBin
      scaleCoef = 1 / step / fromIntegral n
      minV = UV.head vec'
      maxV = UV.last vec'
      step = (maxV - minV) / fromIntegral nBin
      ls = init $ tail $ [minV,(minV+step)..maxV]
      
      vs = toHis ls vec'
      vs' = map (* scaleCoef) vs
      yMax = maximum vs'
      y = -0.025 * yMax
      trueY = max yMax (density standard 0)
      yticks = let yTMax = 100 * (round $ (trueY / scaleCoef) / 100)
                   yts = [yTMax,(yTMax-100)..0] :: [Int]
               in filter ((== 0).(`mod` 200)) yts
      ylabels = map ((* scaleCoef) . fromIntegral) yticks
      is = zip vs' $
           init $ [minV,(minV+step)..maxV]
      xlabels = filter even [floor maxV,(floor maxV-1)..ceiling minV]
      xLabels = map (\xl ->
                     render $ setManyAttrib [("xLabel",show xl)
                                            ,("y",show y)
                                            ,("yDouble", show $! 2*y)] xLabelTemplate
                     ) xlabels
      yLabels = map (\yl ->
                     render $ setManyAttrib [("xNegate",show $ (1.05 * minV - 0.15) )
                                            ,("xMin", show $ 1.05 * minV)
                                            ,("y",show yl)] yLabelTemplate
                     ) ylabels
      
      xTicks = map (\xl ->
                     render $ setManyAttrib [("xLabel",show xl)
                                            ,("yTri",show $! 3.5 * y)] xTickTemplate
                     ) xlabels
      yTicks = map (\(yCoor,yLabel) ->
                     render $ setManyAttrib [("yCoor",show yCoor)
                                            ,("yLabel",show yLabel)
                                            ,("xNegate",show $ -0.3 + minV)] yTickTemplate
                     ) $ zip ylabels yticks
               
      curves = map (\(y,x) ->
                     render $ setManyAttrib [("color",color)
                                            ,("opacity",show opacity)
                                            ,("x1",show x)
                                            ,("x2",show $ x+step)
                                            ,("height",show y)] rectTemplate
                   ) is
  in render $ setManyAttrib
     [("curves",curves)
     ,("xMin",[show (1.01 * minV)])
     ,("xMax",[show (1.01 * maxV)])
     ,("y",[show y])
     ,("title",[title])
     ,("xLabels",xLabels)
     ,("xTicks",xTicks)
     ,("yLabels",yLabels)
     ,("yTicks",yTicks)
     ,("trueXMin",[show $ 1.05 * minV])
     ,("trueY",[show (1.1 * trueY)])
     ,("yMean",[show $ 0.5 * trueY])
     ,("yTitlePos",[show $ 1.35 * minV])
     ,("xTitlePos",[show $ 8*y])
     ,("df",[show df])
     ,("delta",[show delta])
     ,("sigma",[show sigma])
     ,("lx1",[show $ delta + 2*sigma])
     ,("lx2",[show $ delta + 3*sigma])
     ,("ly1",[show $ 0.95 * trueY])
     ,("ly2",[show $ 0.85 * trueY])
     ,("deltaApp",[show $ (fromIntegral $ round $ delta * 100) / 100])
     ,("sigmaApp",[show $ (fromIntegral $ round $ sigma * 100) / 100])
     ,("emStart",[show minV])
     ,("emEnd",[show maxV])] texTemplate

-- | 柱状体高度计算
toHis :: [FloatType] -> UV.Vector FloatType -> [FloatType]
toHis ls' vec' = go [] ls' vec'
  where 
    go acc [] v = reverse $ fromIntegral (UV.length v) : acc
    go acc (l:ls) v =
      let !acc' = fromIntegral (UV.length (UV.takeWhile (<= l) v)) : acc
      in go acc' ls (UV.dropWhile (<= l) v) 


-- | 生成实际z值分布与BH FDR的示意图，可显示频次
hist' :: Int -> Int -> UV.Vector FloatType -> FloatType -> String -> String -> String
hist' nBin df vec opacity color title =
  let vec' = UV.fromList $ sort $ UV.toList vec
      n = UV.length vec
      binLen = fromIntegral n / fromIntegral nBin
      scaleCoef = 1 / step / fromIntegral n
      minV = UV.head vec'
      maxV = UV.last vec'
      step = (maxV - minV) / fromIntegral nBin
      ls = init $ tail $ [minV,(minV+step)..maxV]
      
      vs = toHis ls vec'
      vs' = map (* scaleCoef) vs
      yMax = maximum vs'
      y = -0.025 * yMax
      trueY = max yMax (density standard 0)
      yticks = let yTMax = 100 * (round $ (trueY / scaleCoef) / 100)
                   yts = [yTMax,(yTMax-100)..0] :: [Int]
               in filter ((== 0).(`mod` 200)) yts
      ylabels = map ((* scaleCoef) . fromIntegral) yticks
      is = zip vs' $
           init $ [minV,(minV+step)..maxV]
      xlabels = filter even [floor maxV,(floor maxV-1)..ceiling minV]
      xLabels = map (\xl ->
                     render $ setManyAttrib [("xLabel",show xl)
                                            ,("y",show y)
                                            ,("yDouble", show $! 2*y)] xLabelTemplate
                     ) xlabels
      yLabels = map (\yl ->
                     render $ setManyAttrib [("xNegate",show $ (1.05 * minV - 0.15) )
                                            ,("xMin", show $ 1.05 * minV)
                                            ,("y",show yl)] yLabelTemplate
                     ) ylabels
      
      xTicks = map (\xl ->
                     render $ setManyAttrib [("xLabel",show xl)
                                            ,("yTri",show $! 3.5 * y)] xTickTemplate
                     ) xlabels
      yTicks = map (\(yCoor,yLabel) ->
                     render $ setManyAttrib [("yCoor",show yCoor)
                                            ,("yLabel",show yLabel)
                                            ,("xNegate",show $ -0.3 + minV)] yTickTemplate
                     ) $ zip ylabels yticks
               
      curves = map (\(y,x) ->
                     render $ setManyAttrib [("color",color)
                                            ,("opacity",show opacity)
                                            ,("x1",show x)
                                            ,("x2",show $ x+step)
                                            ,("height",show y)] rectTemplate
                   ) is
  in render $ setManyAttrib
     [("curves",curves)
     ,("xMin",[show (1.01 * minV)])
     ,("xMax",[show (1.01 * maxV)])
     ,("y",[show y])
     ,("xLabels",xLabels)
     ,("xTicks",xTicks)
     ,("title",[title])
     ,("yLabels",yLabels)
     ,("yTicks",yTicks)
     ,("trueXMin",[show $ 1.05 * minV])
     ,("trueY",[show (1.1 * trueY)])
     ,("yMean",[show $ 0.5 * trueY])
     ,("yTitlePos",[show $ 1.35 * minV])
     ,("xTitlePos",[show $ 8*y])
     ,("df",[show df])
     ,("lx1",[show 2])
     ,("lx2",[show 3])
     ,("ly1",[show $ 0.95 * trueY])
     ,("ly2",[show $ 0.85 * trueY])
     ,("emStart",[show minV])
     ,("emEnd",[show maxV])] texTemplate'

texTemplate' :: StringTemplate String
texTemplate' = newSTMP
  "\\\\documentclass{standalone}\n\
   \\\\\usepackage{tikz}\n\
   \\\\\usetikzlibrary{fadings,mindmap,shadows,shapes.arrows,shapes.geometric,\
                    \shapes.misc,matrix,arrows,positioning,calc,decorations.pathreplacing,\
                    \plotmarks}\n\
   \\\\\usepackage{graphicx}\n\
   \\\\\usepackage{xcolor}\n\
   \\\\\usepackage{times}\n\
   \\\\\usepackage{pgfplots}\n\
   \\\\\usepackage{amsbsy}\n\
   \\\\\begin{document}\n\
   \\\\\begin{tikzpicture}\n\
   \\\\\begin{axis}[axis lines=none,\n\
   \title={$title$},\n\
   \clip=false,\n\
   \legend style={draw=none},\n\
   \]\n\
   \$curves;separator='\n'$\n\
   \\\\\addplot[thick,dashed,blue] gnuplot [smooth,no marks,domain=-3.5:3.5] {(1/sqrt(2 * pi)) * exp(-0.5*(x*x))};\n\
   \\\\\draw[thick,dashed,blue] (axis cs:$lx1$,$ly1$) -- (axis cs:$lx2$,$ly1$) node [right] {\\\\small\\\\(H_0: N(0,1) \\\\)};\n\
   \\\\\draw[thick] (axis cs:$xMin$,$y$) -- (axis cs:$xMax$,$y$);\n\
   \\\\\draw[thick,->] (axis cs:$trueXMin$,0) -- (axis cs:$trueXMin$,$trueY$);\n\
   \$xLabels;separator='\n'$\n\
   \$xTicks;separator='\n'$\n\
   \$yLabels;separator='\n'$\n\
   \$yTicks;separator='\n'$\n\
   \\\\\node[rotate=90,font=\\\\bf] at (axis cs: $yTitlePos$,$yMean$) {Frequency};\n\
   \\\\\node[] at (axis cs:0,$xTitlePos$) {\\\\(\\\\boldsymbol{z_i=\\\\Phi^{-1}(F_{$df$}(t_i))}\\\\)};\n\
   \\\\\end{axis}\n\
   \\\\\end{tikzpicture}\n\
   \\\\\end{document}\n"


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
   \\\\\usepackage{amsbsy}\n\
   \\\\\begin{document}\n\
   \\\\\begin{tikzpicture}\n\
   \\\\\begin{axis}[axis lines=none,\n\
   \title={$title$},\n\
   \clip=false,\n\
   \legend style={draw=none},\n\
   \]\n\
   \$curves;separator='\n'$\n\
   \\\\\addplot[thick,dashed,blue] gnuplot [smooth,no marks,domain=-3.5:3.5] {(1/sqrt(2 * pi)) * exp(-0.5*(x*x))};\n\
   \\\\\addplot[thick,red] gnuplot [smooth,no marks,domain=$emStart$:$emEnd$] {1/(sqrt(2*pi)*$sigma$) * exp(-0.5*(x-$delta$)*(x-$delta$)/($sigma$*$sigma$))};\n\
   \\\\\draw[thick,dashed,blue] (axis cs:$lx1$,$ly1$) -- (axis cs:$lx2$,$ly1$) node [right] {\\\\small\\\\(H_0: N(0,1) \\\\)};\n\
   \\\\\draw[thick,red] (axis cs:$lx1$,$ly2$) -- (axis cs:$lx2$,$ly2$) node [right] {\\\\small\\\\(\\\\hat{H_0}: N($deltaApp$,$sigmaApp$) \\\\)};\n\
   \\\\\draw[thick] (axis cs:$xMin$,$y$) -- (axis cs:$xMax$,$y$);\n\
   \\\\\draw[thick,->] (axis cs:$trueXMin$,0) -- (axis cs:$trueXMin$,$trueY$);\n\
   \$xLabels;separator='\n'$\n\
   \$xTicks;separator='\n'$\n\
   \$yLabels;separator='\n'$\n\
   \$yTicks;separator='\n'$\n\
   \\\\\node[rotate=90,font=\\\\bf] at (axis cs: $yTitlePos$,$yMean$) {Frequency};\n\
   \\\\\node[] at (axis cs:0,$xTitlePos$) {\\\\(\\\\boldsymbol{z_i=\\\\Phi^{-1}(F_{$df$}(t_i))}\\\\)};\n\
   \\\\\end{axis}\n\
   \\\\\end{tikzpicture}\n\
   \\\\\end{document}\n"

-- | 将t值转化为对应的标准正态分布下的z值
tToZ :: Int -> FloatType -> FloatType
tToZ !df !t =
  let !dis = studentT $ fromIntegral df
      !dis' = standard -- 标准正态分布 N(0,1)
  in quantile dis' $! cumulative dis t

-- | 由数据直接计算z
zstat :: V.Vector (UV.Vector FloatType)
      -> UV.Vector Bool
      -> UV.Vector FloatType
zstat !expData !label =
  let !df = UV.length label - 2
  in UV.map (tToZ df) $! tstat expData label
 
-- | t值计算
tstat :: V.Vector (UV.Vector FloatType)
      -> UV.Vector Bool
      -> UV.Vector FloatType
tstat !expData !label =
  let idx1 = UV.findIndices id label
      idx2 = UV.findIndices not label
      !n1 = UV.length idx1
      !n2 = UV.length idx2
      !df = UV.length label - 2
      !(m1,v1) = vecFastMeanVar $! V.unsafeBackpermute expData (UV.convert idx1)
      !(m2,v2) = vecFastMeanVar $! V.unsafeBackpermute expData (UV.convert idx2)
      !num = UV.zipWith (-) m1 m2
      !den = UV.map ((* (sqrt $ 1 / fromIntegral n1 + 1 / fromIntegral n2 ))
                     . sqrt . (/ fromIntegral df)) $
             UV.zipWith (+)
             (UV.map (* (fromIntegral $ n1-1)) v1)
             (UV.map (* (fromIntegral $ n2-1)) v2)
  in UV.zipWith (/) num den




-- | 期望最大化拟合的Bayes FDR参数，包括p0与hatH_0的均值与方差
mleFitting :: UV.Vector FloatType -> FloatType -> (FloatType,FloatType,FloatType)
mleFitting zs z0 =
  let cdf = cumulative standard
      h0 delta0 sigma0 = cdf ((z0 - delta0) / sigma0) - cdf ((-z0 - delta0) / sigma0)
      phi0 z' delta0 sigma0 =
        let d = normalDistr delta0 sigma0
        in density d z'
      n = fromIntegral $ UV.length zs 
      idx0 = UV.findIndices ((<= z0) . abs) zs
      n0 = fromIntegral $ UV.length idx0
      zs0 = UV.unsafeBackpermute zs idx0
      f theta delta0 sigma0 =
        let v = negate $
                n0 * (log theta) +
                (n-n0) * log (1 - theta) + (
                  UV.foldl1' (+) $
                  UV.map
                  (\z -> log (phi0 z delta0 sigma0) - log (h0 delta0 sigma0)) zs0)
        in v
      (t:d:s:[]) = fst $ minimize NMSimplex2 1e-6 500 [0.49999,2,2.99999] (\(x:y:z:_) -> f x y z) [0.5,0,3]
      p0 = t / h0 d s
  in (d,s,p0)



