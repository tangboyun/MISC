{-# LANGUAGE BangPatterns #-}
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
module CoxScore 
       (
         coxScore
       )
       
       where

import           Control.Exception
import           Control.Parallel.Strategies
import           Data.List
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
 
type FloatType = Double
type ExpData = V.Vector (UV.Vector FloatType)
type SurvivalTime = UV.Vector FloatType
type FailureVec = UV.Vector Bool
type Score = UV.Vector FloatType

{-# INLINE coxScore #-}
coxScore :: ExpData -> SurvivalTime -> FailureVec -> Score
coxScore chipDatas survivalT failVec = 
  assert (V.length chipDatas == UV.length survivalT &&
          V.length chipDatas == UV.length failVec) $
  UV.imap (\i e ->
            let ne = numerator `atUV` i
            in ne / (sqrt e)) denominator
  where
    atUV :: UV.Unbox a => UV.Vector a -> Int -> a
    atUV = UV.unsafeIndex
    atV :: V.Vector a -> Int -> a
    atV = V.unsafeIndex
    nSample = UV.length survivalT
    nGene = UV.length $! chipDatas `atV` 0
    failIdxs = UV.findIndices id failVec
    uniqueDT = UV.fromList $! 
               nub $ sort $ UV.toList $ 
               UV.unsafeBackpermute survivalT failIdxs
    nUniqueDT = UV.length uniqueDT
    idxsAboveEachDT = V.map (\e -> UV.findIndices (>= e) survivalT) $! 
                       UV.convert uniqueDT
    numAtEachDT = UV.convert $! V.map UV.length idxsAboveEachDT
    ls = let tmpIdxs = UV.enumFromN 0 nSample
          in map (\i -> 
                   let deathTime = uniqueDT `atUV` i
                       !idxs = UV.findIndices 
                               (\idx -> 
                                 survivalT `atUV` idx == deathTime && 
                                 failVec `atUV` idx) tmpIdxs
                       !d'   = UV.length idxs
                       !s'   = V.foldl1' (UV.zipWith (+)) $! 
                               V.unsafeBackpermute chipDatas 
                               (UV.convert idxs)
                   in P i d' s') [0..nUniqueDT-1]
    (numerator,denominator) =        
      foldl' (\(accN,accD) (P i d s) ->
               let idxs = idxsAboveEachDT `atV` i
                   d' = fromIntegral d
                   n = fromIntegral $ numAtEachDT `atUV` i
                   !x = V.unsafeBackpermute chipDatas 
                        (UV.convert idxs)
                   !sx = V.foldl1' (UV.zipWith (+)) x
                   ss = V.foldl1' (UV.zipWith (+)) $ 
                        V.map (\e -> UV.map (^^2) e) x
                   nu = UV.zipWith (+) accN $
                        UV.zipWith (-) s $ 
                        UV.map 
                        (* (d' / fromIntegral 
                            (numAtEachDT `atUV` i))) sx
                   de = UV.zipWith (+) accD $
                        UV.map (* (d' / n)) $
                        UV.zipWith (-) ss $ 
                        UV.map ((/n).(^^2)) sx  
               in (nu,de) `using` parTuple2 rseq rseq
             ) (UV.replicate nGene 0,UV.replicate nGene 0) ls

data PIIV = P {-# UNPACK #-} !Int
              {-# UNPACK #-} !Int
            !(UV.Vector FloatType)
                        
            

