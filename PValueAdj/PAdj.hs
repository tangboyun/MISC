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
--             Mitchell Guttman (2011) lincRNAs act in the circuitry controlling pluripotency and differentiation 
--             Nature 477, 295–300
--
--             Finding consistent patterns: a nonparametric approach for identifying differential expression in RNA-Seq data
--             Jun Li, Robert Tibshirani
--
-----------------------------------------------------------------------------
module PValueAdj where

import           Control.Monad.ST
import           Control.Parallel.Strategies
import           Data.Function
import           Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Debug.Trace
import           Statistics.Basic
import           Statistics.Sampling
import           Statistics.Distribution
import           Statistics.Distribution.ChiSquared
import           Statistics.Distribution.StudentT
import           System.Random.MWC

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

{-# INLINE rankAll' #-}
-- | 返回rank,即为原数组中小于该值的个数,不处理相同值
rankAll' :: UV.Vector FloatType -> UV.Vector FloatType  
rankAll' !vec =
  let !n = UV.length vec
  in UV.fromList $
     map (fromIntegral . snd) $
     sortBy (compare `on` fst) $ fst $
     foldl' (\(acc,count) ls ->
              let !acc' = map (\e -> (e,count)) ls ++ acc
                  !count' = count + length ls
              in (acc',count')
              ) ([],0) $
     groupBy ((==) `on` (vec `at`)) $ 
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


-- | q-value for pFDR 这个算法需要提供太多初始参数，推荐用permFDR
-- qValue :: Seed                -- ^ initial random seed
--        -> UV.Vector FloatType -- ^ range used to find the optimal pi0
--        -> UV.Vector FloatType -- ^ input p-value
--        -> (UV.Vector FloatType,Seed)
-- qValue seed lambda' pVec =
--   let m = UV.length pVec
--       n = 100
--       l = UV.length lambda
--       f ls ps = UV.map
--                 (\lam ->
--                   (fromIntegral $ UV.length $
--                    UV.findIndices (>= lam) ps) /
--                   fromIntegral m / ( 1 - lam)
--                 ) ls
--       validIdx = UV.findIndices (\e -> e > 0 && e < 1) $ f lambda' pVec
--       lambda = UV.unsafeBackpermute lambda' validIdx
--       pi0s = f lambda pVec
--       minPi0 = minimum $
--                UV.toList $ UV.filter (\e -> e >= 0 && e < 1) pi0s
--       (bootMSE,seed') =
--         foldl'
--         (\(acc,s) _ ->
--           let (bootIdx,s') = runST $ do
--                 gen <- initialize $ fromSeed s
--                 idxs <- UV.replicateM m $
--                         uniformR (0,l-1) gen
--                 s'' <- save gen
--                 return $ (idxs,s'')
--               -- sampling with replacement
--               bootPVec = UV.unsafeBackpermute pVec bootIdx
--               pi0Boot = f lambda bootPVec
--               acc' = UV.zipWith (+) acc $
--                      UV.map ((^^(2::Int)).(\p -> p - minPi0)) pi0Boot 
--           in (acc',s') 
--         ) (UV.replicate l 0,seed) [0 .. n-1]
--       piIdx = minimumBy (compare `on` (bootMSE `UV.unsafeIndex`)) [0..l-1]
--       myPi = min 1 $ pi0s `UV.unsafeIndex` piIdx                                 
--       v = rankAll pVec
--   in trace (show myPi ++ show pi0s) $ (UV.map ((*myPi).(* fromIntegral m)) $ UV.zipWith (/) pVec v,seed')


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


ttestTwoTail :: V.Vector (UV.Vector FloatType)
             -> UV.Vector Bool
             -> UV.Vector FloatType
ttestTwoTail !expData !label =
  let !dis = studentT $ fromIntegral $ UV.length label -2
      !ts = UV.map abs $ tstat expData label
  in UV.map (\t -> 2 * cumulative dis (negate t)) ts

ttestLT :: V.Vector (UV.Vector FloatType)
             -> UV.Vector Bool
             -> UV.Vector FloatType
ttestLT !expData !label =
  let !dis = studentT $ fromIntegral $ UV.length label -2
  in UV.map (cumulative dis) $ tstat expData label

ttestRT :: V.Vector (UV.Vector FloatType)
             -> UV.Vector Bool
             -> UV.Vector FloatType
ttestRT !expData !label = UV.map (1 -) $
                          ttestLT expData label


median :: UV.Vector FloatType -> FloatType
median vec =
  let l = UV.length vec
      vec' = UV.fromList $ sort $ UV.toList vec
  in if odd l
     then vec' `UV.unsafeIndex` (l `div` 2)
     else let l2 = l `div` 2
              l1 = l2 - 1
          in 0.5 * (vec' `UV.unsafeIndex` l1 +
                    vec' `UV.unsafeIndex` l2)

-- | 基于permutation的FDR校正
permFDR :: Seed
        -> Int
        -> (V.Vector (UV.Vector FloatType)
           -> UV.Vector Bool
           -> UV.Vector FloatType)    -- ^ func for t-static calc
        -> V.Vector (UV.Vector FloatType)
        -> UV.Vector Bool
        -> (UV.Vector FloatType,Seed)
permFDR seed n f expData label =
  let !ts = f expData label
      !rs = rankAll' $ UV.map (negate . abs) ts
      nT = UV.length ts
      idx1 = UV.findIndices id label
      l = UV.length label
      ls = [0..l-1]
      l1 = UV.length idx1
      !(rndLabels,seed') = if comb (fromIntegral l1) (fromIntegral l) < fromIntegral n -- 是否达到理论上限
                           then let !rndLs = map (\idxs -> UV.fromList $ map (`elem` idxs) ls) $ combinations l1 ls
                                in (rndLs,seed)
                           else foldl'
                                (\(acc,s) _ ->
                                  let !(rndLabel,s') = shuffle s label
                                  in (rndLabel:acc,s')
                                ) ([],seed) [1..n]
      rndTs = parMap rseq (f expData) rndLabels             
      !fdrs' = UV.imap (\i _ ->
                         let t = abs $ ts `UV.unsafeIndex` i
                             r = rs `UV.unsafeIndex` i
                             vs = map (fromIntegral . UV.length .
                                       UV.findIndices ((> t) . abs)) rndTs
                             v = (\(a,leng) ->  a / leng) $!
                                 foldl' (\(acc,len) e ->
                                          let acc' = acc + e
                                              len' = len + 1
                                          in (acc',len')) (0,0) vs
                         in v / r
                       ) $ UV.replicate nT (0::FloatType)
      !m = median $ foldr ((UV.++) `on` (UV.map abs)) UV.empty rndTs
      !pi = 2 * ( 1 / fromIntegral nT) * (fromIntegral $!
                                          UV.length $!
                                          UV.findIndices ((<= m) . abs) ts)
  in (UV.map (* pi) fdrs',seed')

permFDR' :: Seed
        -> Int
        -> (V.Vector (UV.Vector FloatType)
           -> UV.Vector Bool
           -> UV.Vector FloatType)  -- ^ func for p-value calc
        -> V.Vector (UV.Vector FloatType)
        -> UV.Vector Bool
        -> (UV.Vector FloatType,Seed)
permFDR' seed n fp expData label =
  let !ps = fp expData label
      !rs = rankAll' ps
      nT = UV.length ps
      idx1 = UV.findIndices id label
      l = UV.length label
      ls = [0..l-1]
      l1 = UV.length idx1
      (rndLabels,seed') = if comb (fromIntegral l1) (fromIntegral l) < fromIntegral n -- 是否达到理论上限
                          then let !rndLs = map (\idxs -> UV.fromList $ map (`elem` idxs) ls) $ combinations l1 ls
                               in (rndLs,seed)
                          else foldl'
                               (\(acc,s) _ ->
                                  let !(rndLabel,s') = shuffle s label
                                  in (rndLabel:acc,s')
                               ) ([],seed) [1..n]
      !rndPs = parMap rseq (fp expData) rndLabels             
      !fdrs' = UV.imap (\i _ ->
                         let p = ps `UV.unsafeIndex` i
                             r = rs `UV.unsafeIndex` i
                             vs = map (fromIntegral . UV.length .
                                       UV.findIndices (< p)) rndPs
                             v = (\(a,leng) ->  a / leng) $!
                                 foldl' (\(acc,len) e ->
                                          let acc' = acc + e
                                              len' = len + 1
                                          in (acc',len')) (0,0) vs
                         in v / r
                       ) $ UV.replicate nT (0::FloatType)
      !m = median $ foldr (UV.++) UV.empty rndPs
      !pi = 2 * ( 1 / fromIntegral nT) * (fromIntegral $
                                          UV.length $
                                          UV.findIndices (>= m) ps)
  in (UV.map (* pi) fdrs',seed')


comb :: FloatType -> FloatType -> Integer
comb !a !b =
  let c = b - a
      num = foldl1' (+) $ map log [1..b]
      den = foldl1' (+) $ map log $ [1..a] ++ [1..c]
  in round $ exp $ num - den
     
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']
