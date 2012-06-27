-----------------------------------------------------------------------------
-- |
-- Module : Nearest Shrunken Centroids Classifier
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference : Class Prediction by Nearest Shrunken Centroids, with Applications to DNA Microarrays
--	Robert Tibshirani, Trevor Hastie, Balasubramanian Narasimhan, and Gilbert Chu
--	Statist. Sci. Volume 18, Issue 1 (2003), 104-117.
-- 
--
-----------------------------------------------------------------------------

module Classification.NSC
       
       where

import Statistics.Basic
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import Control.Parallel.Strategies
import Data.Function
import Data.Tuple

type FloatType = Double
type Sample = V.Vector (UV.Vector FloatType)
type Label = UV.Vector Int

  
sampleProperty :: Sample
               -> Label
               -> (UV.Vector FloatType,UV.Vector FloatType,[(FloatType,FloatType,UV.Vector FloatType)]) 
sampleProperty sample label =
  let ts = sortBy (compare `on` snd) $ UV.toList $ UV.indexed label
      -- idxs for each label
      is = map (UV.fromList . (map fst)) $ groupBy ((==) `on` snd) $ ts
      ls = nub $ map (fromIntegral . snd) ts
      ks = map (fromIntegral . UV.length) is
      nSample = fromintegral $ UV.length label
      nClass = length ls
      -- mean over all classes
      mean = UV.map (/ nSample) $ V.foldl1' (UV.zipWith (+)) sample
      -- mean and res SSE for each class
      (ms,ress) = unzip $ 
                  map (\idxs -> 
		    let n = fromIntegral $ UV.length idxs
                        (m,var) = vecFastMeanVar $ 
                                  V.unsafeBackpermute sample idxs
			res = UV.map (* (n - 1)) var
		    in (m,res)) is
      -- pooled Std.
      std = UV.map (sqrt . (/ (nSample - nClass))) $
            UV.foldl1' (+) ress
      in (mean,std,zip3 ls ks ms)

modifyDis delta dVec =
  UV.map (\d ->
  if abs d > delta
  then if d > 0
       then d - delta
       else negate $ 
            abs d - delta
  else 0) dVec

       
predict test (mean,std,ms) regShr regStd theta prior =
  let std' = UV.map (+ regStd) std
      ns = map (\(_,k,_) -> k) ms
      ls = map (\(l,_,_) -> l) ms
      n = foldl' (+) 0 ns
      ds' = map 
            (modifyDis regShr . (\(_,k,m) th-> 
            UV.zipWith (/) 
            (UV.zipWith (-) m mean) 
            (UV.map ((* (sqrt $ 1/k - 1/n)) . (* th)) std'))) $ 
            zip ms $ UV.toList theta    
      ms' = zip3 ls (UV.toList prior) $ 
            map (\(_,k,d') -> 
                  UV.zipWith (+) mean 
                  (UV.map ((sqrt $ 1/k - 1/n) *) $
                   UV.zipWith (*) std' d')) $ 
            zip ns ds'
  in 
      
predict_impl test ms' =         
