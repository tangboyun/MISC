{-# LANGUAGE BangPatterns,RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference : Storey JD. (2002) A direct approach to false discovery rates.
--             Journal of the Royal Statistical Society, Series B, 64: 479-498.
-- 
--
-----------------------------------------------------------------------------

module PValueAdj where

import           Data.Function
import           Data.List
import qualified Data.Vector.Unboxed as UV
import           Statistics.Distribution
import           Statistics.Distribution.ChiSquared
import System.Random.MWC
import Control.Monad.ST
import Debug.Trace

type FloatType = Double

{-# INLINE rankAll #-}
-- | 等价于R中的rank，平均处理相同值的rank
rankAll :: UV.Vector FloatType -> UV.Vector FloatType  
rankAll !vec =
  let !n = UV.length vec
  in UV.fromList $! map fst $
     sortBy (compare `on` snd) $ 
     concatMap
     (\ls -> 
       let (acc_sum,acc_len) =
             foldl' (\(acc_s,acc_l) e -> 
                      let !acc_s' = acc_s + e
                          !acc_l' = acc_l + 1
                      in (acc_s',acc_l')
                    ) (0,0) $ map fst ls
           !meanIdx = acc_sum / acc_len
       in map (\(_,v) -> (meanIdx,v)) ls) $ 
     groupBy (\(_,v1) (_,v2) -> vec `at` v1 == vec `at` v2) $ zip [1..] $
     sortBy (compare `on` (vec `at`)) [0..n-1]
  where 
    at = UV.unsafeIndex

{-# INLINE bhFDR #-}
bhFDR :: UV.Vector FloatType -> UV.Vector FloatType
bhFDR !pVec = 
  let !nSample = fromIntegral $ UV.length pVec
  in UV.map (\e -> if e > 1 then 1 else e) $
     UV.zipWith (/) (UV.map (* nSample) pVec) $ rankAll pVec



bhFDR' :: FloatType -> UV.Vector FloatType -> UV.Vector Int
bhFDR' !alpha !pVec =
  let (idxV,pSorted) = UV.unzip $ UV.fromList $ sortBy (compare `on` snd) $ UV.toList $ UV.indexed pVec
      len = fromIntegral $ UV.length pVec
      firstN = UV.length $ UV.takeWhile id $
               UV.map (< alpha) $
               UV.zipWith (/) (UV.map (* len) pSorted) (rankAll pSorted)
  in UV.take firstN idxV
     
{-# INLINE bonferroni #-}
bonferroni :: UV.Vector FloatType -> UV.Vector FloatType
bonferroni !pVec = UV.map (\e -> if e > 1 then 1 else e) $
                   UV.map (* (fromIntegral $ UV.length pVec)) pVec


-- | q-value for pFDR
qValue :: Seed                -- ^ initial random seed
       -> UV.Vector FloatType -- ^ range used to find the optimal pi0
       -> UV.Vector FloatType -- ^ input p-value
       -> (UV.Vector FloatType,Seed)
qValue seed lambda' pVec =
  let m = UV.length pVec
      n = 100
      l = UV.length lambda
      f ls ps = UV.map
                (\lam ->
                  (fromIntegral $ UV.length $
                   UV.findIndices (>= lam) ps) /
                  fromIntegral m / ( 1 - lam)
                ) ls
      validIdx = UV.findIndices (\e -> e > 0 && e < 1) $ f lambda' pVec
      lambda = UV.unsafeBackpermute lambda' validIdx
      pi0s = f lambda pVec
      minPi0 = minimum $
               UV.toList $ UV.filter (\e -> e >= 0 && e < 1) pi0s
      (bootMSE,seed') =
        foldl'
        (\(acc,s) _ ->
          let (bootIdx,s') = runST $ do
                gen <- initialize $ fromSeed s
                idxs <- UV.replicateM m $
                        uniformR (0,l-1) gen
                s'' <- save gen
                return $ (idxs,s'')
              -- sampling with replacement
              bootPVec = UV.unsafeBackpermute pVec bootIdx
              pi0Boot = f lambda bootPVec
              acc' = UV.zipWith (+) acc $
                     UV.map ((^^(2::Int)).(\p -> p - minPi0)) pi0Boot 
          in (acc',s') 
        ) (UV.replicate l 0,seed) [0 .. n-1]
      piIdx = minimumBy (compare `on` (bootMSE `UV.unsafeIndex`)) [0..l-1]
      myPi = min 1 $ pi0s `UV.unsafeIndex` piIdx                                 
      v = rankAll pVec
  in trace (show myPi ++ show pi0s) $ (UV.map ((*myPi).(* fromIntegral m)) $ UV.zipWith (/) pVec v,seed')
