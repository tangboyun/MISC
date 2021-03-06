{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference : Efficient Quadratic Regularization for Expression Arrays
--             Trevor Hastie,Robert Tibshirani
--             与R的MASS包中的lm.ridge计算结果会有少许差异,R包中使用X_stand=X/(mean((X-mean(X))^2))^0.5
--             这里用的是标准的方差求法。
--
-----------------------------------------------------------------------------

module Ridge where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util
import Data.Function
import Data.List

type FloatType = Double



-- | each col has zero mean
centering :: Matrix FloatType -> Matrix FloatType
centering !m =
  let n = rows m
  in m `sub` scale (1 / fromIntegral n) (ones n n <> m)

standization :: Matrix FloatType -> Matrix FloatType
standization !m =
  let !m' = centering m
      !n = rows m
  in m' `divide` (ones n 1 <> cmap sqrt (scale (1 / fromIntegral (n-1)) (ones 1 n <> (m' `mul` m'))))



-- | Return beta. Estimate lambda using generalized cross validation
ridgeReg :: Matrix FloatType -> Matrix FloatType -> [FloatType] -> Matrix FloatType
ridgeReg expMatrix resVec ls =
  let m = standization expMatrix
      y = centering resVec
      n = rows m
      (u,s,v) = fullSVD m
      r = u <> s
      lambdas = map (2 **) ls
      gcv lambda =
        let h = m <> v <> pinv ((trans r <> r) `add` scale lambda (ident n)) <> trans r
            yhat = h <> y
            rssY = y `sub` yhat
            num = sumElements (trans rssY <> rssY) / fromIntegral n
            den = (1 - foldVector (+) 0 (takeDiag h) / fromIntegral n) ^^ 2
        in num / den
      l = minimumBy (compare `on` gcv) lambdas
  in v <> pinv ((trans r <> r) `add` scale l (ident n)) <> trans r


-- | Return the predicted response value.
ridgeRegPredict :: Matrix FloatType -> Matrix FloatType -> Matrix FloatType -> [FloatType] -> Matrix FloatType
ridgeRegPredict trainMatrix trainResponse testMatrix paras =
  let beta = ridgeReg trainMatrix trainResponse paras
      nTrain = rows trainResponse
      beta0 = sumElements trainResponse / fromIntegral nTrain
      m = centering trainMatrix
      nTest = rows testMatrix
      -- test - colMean of train / colStd of train
      scaledTestMatrix = testMatrix `sub` scale (1 / fromIntegral nTrain) (ones nTest nTrain <> trainMatrix) `divide`
                         (ones nTest 1 <> cmap sqrt (scale (1 / fromIntegral (nTrain-1)) (ones 1 nTrain <> (m `mul` m))))
  in beta0 `addConstant` (scaledTestMatrix <> beta)
