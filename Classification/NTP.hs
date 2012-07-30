{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module : Nearest Template Prediction
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference : Hoshida Y (2010) Nearest Template Prediction: 
--             A Single-Sample-Based Flexible Class Prediction with Confidence Assessment. 
--             PLoS ONE 5(11): e15543. doi:10.1371/journal.pone.0015543
-- 
--
-----------------------------------------------------------------------------
module NTP 
       (
         ntp
       , ntpWithPermTest
       , ntpPredict
       )
       where

import           Control.Parallel.Strategies
import           Data.List
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import           PValueAdj.PAdj
import           Statistics.Sampling
import           System.Random.MWC
import           SurvivalAnalysis.CoxScore
type FloatType = Double

data P3 = P {-# UNPACK #-} !FloatType
            {-# UNPACK #-} !FloatType
            {-# UNPACK #-} !FloatType

{-# INLINE cosSim #-}
cosSim :: UV.Vector FloatType -> UV.Vector FloatType -> FloatType
cosSim !v1 !v2 = 
  let !(P nu de1 de2) =
        UV.foldl' 
        (\(P n1 d1 d2) (x1,x2) -> 
          let n1' = n1 + x1*x2 
              d1' = d1 + x1*x1
              d2' = d2 + x2*x2
          in P n1' d1' d2'
        ) (P 0 0 0) $ UV.zip v1 v2
  in nu / sqrt (de1 * de2)


{-# INLINE cosDis #-}
cosDis :: UV.Vector FloatType -> UV.Vector FloatType -> FloatType
cosDis = ((1 -) .) . cosSim

{-# INLINE ntp #-}
ntp :: UV.Vector FloatType -- ^ Cox Score in this case
    -> UV.Vector FloatType -- ^ Normalized expression value
    -> (Int,(FloatType,FloatType)) -- ^ label and dis to each template, +1: close to posTem -1: close to negTem
ntp !weight !sample = 
  let !sample' = UV.zipWith (*) sample $ UV.map abs weight
      !posTemplate = weight
      !negTemplate = UV.map negate posTemplate
      !dis1 = cosDis sample' posTemplate
      !dis2 = cosDis sample' negTemplate
      !label = if dis1 < dis2 
               then 1
               else -1
  in (label,(dis1,dis2))


{-# INLINE ntpWithPermTest #-}
ntpWithPermTest :: Int   -- ^ repeat times for permutatiion test
                -> Seed  -- ^ initial random seed
                -> UV.Vector Int -- ^ indices for selected signature
                -> UV.Vector FloatType -- ^ weight for all genes
                -> UV.Vector FloatType -- ^ exp data for all genes
                -> ((Int,FloatType),Seed) -- ^ label with nominal p-value
ntpWithPermTest !n !seed !idxVec !w !sample =
  let sample' = UV.unsafeBackpermute sample idxVec -- signature
      w' = UV.unsafeBackpermute w idxVec           -- sig's cox
      (!label,!(dis1,dis2)) = ntp w' sample'
      (!rnd_idx,!seed') = 
        foldl' 
        (\(acc,s) _ ->
          let !(v,s') = shuffle s $ UV.enumFromN 0 $ 
                        UV.length sample
              !ls = UV.take (UV.length idxVec) v : acc
          in (ls,s'))
        ([],seed) [0..n-1]
      rand_dis = UV.fromList $ 
                 parMap rdeepseq
                 (\idxV ->
                   let !sam = UV.unsafeBackpermute sample idxV -- random genes
                       !d = if label == 1
                            then cosDis w' sam  -- Pos sample compare with pos template
                            else cosDis (UV.map negate w') sam
                   in d) rnd_idx
      dis = if label == 1
            then dis1
            else dis2
      !p = (UV.head $! rankAll $ dis `UV.cons` rand_dis) / fromIntegral (UV.length rand_dis + 1) 
  in ((label,p),seed')


{-# INLINE ntpWithPermTest' #-}
ntpWithPermTest' :: Int   -- ^ repeat times for permutatiion test
                -> Seed  -- ^ initial random seed
                -> UV.Vector Int -- ^ indices for selected signature
                -> UV.Vector FloatType -- ^ weight for all genes
                -> UV.Vector FloatType -- ^ exp data for all genes
                -> ((Int,FloatType),(FloatType,UV.Vector FloatType),Seed) -- ^ label with nominal p-value , Distance and Random distances
ntpWithPermTest' !n !seed !idxVec !w !sample =
  let sample' = UV.unsafeBackpermute sample idxVec -- signature
      w' = UV.unsafeBackpermute w idxVec           -- sig's cox
      (!label,!(dis1,dis2)) = ntp w' sample'
      (!rnd_idx,!seed') = 
        foldl' 
        (\(acc,s) _ ->
          let !(v,s') = shuffle s $ UV.enumFromN 0 $ 
                        UV.length sample
              !ls = UV.take (UV.length idxVec) v : acc
          in (ls,s'))
        ([],seed) [0..n-1]
      rand_dis = UV.fromList $ 
                 parMap rdeepseq
                 (\idxV ->
                   let !sam = UV.unsafeBackpermute sample idxV -- random genes
                       !d = if label == 1
                            then cosDis w' sam  -- Pos sample compare with pos template
                            else cosDis (UV.map negate w') sam
                   in d) rnd_idx
      dis = if label == 1
            then dis1
            else dis2
      !p = (UV.head $! rankAll $ dis `UV.cons` rand_dis) / fromIntegral (UV.length rand_dis + 1) 
  in ((label,p),(dis,rand_dis),seed')


-- | 使用leave-one-out赋予label，同时计算permutation下的p与q-value
loocvNTPWithPerm :: Int -- ^ permutation times (permutation on signature)
         -> Seed        
         -> UV.Vector Int -- ^ indices for selected signature
         -> V.Vector (UV.Vector FloatType) -- ^ exp data for all genes
         -> UV.Vector FloatType            -- ^ survival time
         -> UV.Vector Bool                 -- ^ failure indicator
         -> ([(Int,FloatType,FloatType)],Seed)
loocvNTPWithPerm nPerm seed idxVec expData survTime failVec =
  let nSample = V.length expData
      result = foldl' (\(acc,s) idx ->
                        let  expData' = V.unsafeBackpermute expData $
                                        V.findIndices (/= idx) $ V.enumFromN 0 nSample
                             w = coxScore expData' survTime failVec
                             ((l,p),(d,randD),s') = ntpWithPermTest' nPerm s
                                                    idxVec w (expData `V.unsafeIndex` idx)
                             acc' = acc ++ [(l,p,d,randD)]
                        in (acc',s')
                      ) ([],seed) [0..nSample-1]
      
      (ls,ps,ds,randDs) = unzip4 $ fst $ result
      seed' = snd result
      randDis = foldr (UV.++) UV.empty randDs
      dm = median randDis
      rs = rankAll' $ UV.fromList ds
      pi = 2 * ( 1 / fromIntegral nSample) * (fromIntegral $ length $ filter (>= dm) ds)
      vs = map ((/ fromIntegral nSample) . (\d -> fromIntegral $ UV.length $ UV.findIndices (< d) randDis) ds
      qs_orig = zipWith ((* pi).(\v r -> if r == 0 then v else v/r)) vs rs
      idxV = map fst $ sortBy (compare `on` snd) $ zip [0..] ps
      idxV' = UV.fromList $ map fst $ sortBy (compare `on` snd) $ zip [0..] idxV
      qsOrdered = UV.toList $ UV.unsafeBackpermute (UV.fromList qs_orig) idxV
      qs = UV.unsafeBackpermute ((head qsOrdered) : reorder (head qsOrdered) (tail qsOrdered)) idxV'
  in (zip3 ls ps qs,seed')

ntpPredict :: FloatType -- ^ FDR control
           -> Int       -- ^ repeat times for permutation test
           -> Seed      -- ^ initial random seed
           -> UV.Vector Int  -- ^ indices for selected signature
           -> UV.Vector FloatType -- ^ weight for all genes
           -> ExpData             -- ^ exp data for unlabeled sample
           -> ((UV.Vector Int,UV.Vector Int),Seed) -- ^ true pos idx, neg idx
ntpPredict !fdrCutOff !nPerm !seed !idxVec !w !normalizedExpData =
  let (!labelInfos,seed') = V.foldl'
                           (\(acc,s) sample ->
                             let !((l,p),s') = ntpWithPermTest nPerm s idxVec w sample
                                 !acc' = acc ++ [(l,p)]
                             in (acc',s')
                           ) ([],seed) normalizedExpData
      (!labels,!ps) = unzip labelInfos
      !lVec = UV.fromList labels
      !pVec = UV.fromList ps
      !posIdxs = UV.findIndices (== 1) lVec
      -- !truePosIdxs = UV.findIndices (< fdrCutOff) $
      --                bhFDR $ UV.unsafeBackpermute pVec posIdxs
      
      !truePosIdxs = bhFDR' fdrCutOff $ UV.unsafeBackpermute pVec posIdxs
--      !negIdxs = UV.fromList $ [0..length labels - 1] \\ (UV.toList truePosIdxs)
      !negIdxs = UV.fromList $ [0..length labels - 1] \\ (UV.toList posIdxs)
  in trace (show $ bhFDR $ UV.fromList $ sort $ UV.toList $ UV.unsafeBackpermute pVec posIdxs) --((truePosIdxs,negIdxs),seed')
     ((posIdxs,negIdxs),seed')
