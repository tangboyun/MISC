-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference :   George W. Wright and Richard M. Simon
--               A random variance model for detection of differential gene
--               expression in small microarray experiments
--               Bioinformatics (2003) 19(18): 2448-2455
--               doi:10.1093/bioinformatics/btg345
-----------------------------------------------------------------------------
module RVM where

import Numeric.LinearAlgebra
import Numeric.Container
import qualified Data.Vector as V
import Control.Exception
import Numeric.GSL.Distribution.Continuous
import Numeric.GSL

type FloatType = Double 
type ExpData = V.Vector (Vector FloatType)

data TestType = TwoTail 
              | LeftTail
              | RightTail  
              deriving (Eq,Show)
         
designMatrix :: [Int] -> Matrix FloatType          
designMatrix ns = 
  let l = length ns
  in fromLists $ 
     concatMap 
     (\(row,n) -> 
       replicate n (replicate row 0 ++ 
                    [1.0] ++ 
                    replicate (l - row - 1) 0)
     ) $ zip [0..] ns
  
getPfromT :: Vector FloatType -> FloatType -> TestType -> Vector FloatType
getPfromT tV df tp =
  let pf =
        case tp of
          TwoTail -> (\e -> if e > 1 then 1 else e ) . 
                     (2 *) . 
                     density_1p TDist Lower df . 
                     negate . abs
          LeftTail -> (\e -> if e > 1 then 1 else e ) .
                      density_1p TDist Lower df . 
                      negate
          RightTail -> (\e -> if e > 1 then 1 else e ) .
                       density_1p TDist Lower df 
  in cmap pf tV

rvmTTest :: ExpData -> ExpData -> TestType -> Vector FloatType
rvmTTest group1 group2 tp = getPfromT tCorrected (fromIntegral df) tp
  where                        
    n1 = V.length group1
    n2 = V.length group2
    df = n1 + n2 - 2
    stepSize = 0.1
    precision = 1e-6
    maxIter = 1000
    startP = [1.0,1.0]
    resSum = 
      let x = designMatrix [n1,n2]
          y = fromColumns $ V.toList group1 ++ V.toList group2
          betaV = pinv (trans x <> x) <> trans x <> y
          r = y `sub` x <> betaV 
      in fromList $ 
         map (foldVector (+) 0) $ 
         toRows $ r `mul` r
    estVar = cmap (/ fromIntegral df) resSum
    fitF a b = -- a*b*varFit ~ F((n-2),2a) 
      let v = a * b
          f = negate . (+ log v) . log . 
              density_2p FDist Density (fromIntegral df) (2*a) . ( * v)
      in -- sum(-(log(fpdf(varFit*(arg(1)*arg(2)),df,2*arg(1)))+log(arg(1)*arg(2))));
       foldVector (+) 0 $ cmap f estVar 
    calcPartialDeriv [a,b] = 
      let derivA = fst $ derivCentral stepSize (flip fitF b) a 
          derivB = fst $ derivCentral stepSize (fitF a) b
      in [derivA,derivB]
    fWrap [a,b] = fitF a b
    [estA,estB] = fst $ 
                  minimizeD VectorBFGS2 
                  precision maxIter stepSize 
                  precision fWrap calcPartialDeriv startP
    varCorrected = cmap ((/ (fromIntegral df + 2 * estA)) . (+ 1 / estB)) resSum
    vecMean = map (\e -> foldVector (+) 0 e / fromIntegral (dim e)) . V.toList
    tCorrected = zipVectorWith (/)
                 (fromList $ zipWith (-) (vecMean group1) (vecMean group2)) 
                 (cmap (sqrt . (* (1 / fromIntegral n1 + 1 / fromIntegral n2))) varCorrected)
