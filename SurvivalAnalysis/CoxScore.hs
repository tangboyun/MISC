{-# LANGUAGE BangPatterns,FlexibleContexts #-}
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
       , coxPermTest
       )
       
       where

import           Control.Exception
import           Control.Parallel.Strategies
import           Data.List
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import Statistics.Sample
import System.Random.MWC
import Control.Monad
import Control.Monad.ST

type FloatType = Double
type ExpData = V.Vector (UV.Vector FloatType)
type SurvivalTime = UV.Vector FloatType
type FailureVec = UV.Vector Bool
type Score = UV.Vector FloatType
type PValues = UV.Vector FloatType

coxScore :: ExpData -> SurvivalTime -> FailureVec -> Score
coxScore !chipDatas !survivalT !failVec = 
  assert (V.length chipDatas == UV.length survivalT &&
          V.length chipDatas == UV.length failVec) $!
  UV.imap (\i e ->
            let ne = numerator `atUV` i
            in ne / sqrt e) denominator
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
                        V.map (UV.map (^^ (2 :: Int))) x
                   !nu = UV.zipWith (+) accN $
                         UV.zipWith (-) s $ 
                         UV.map 
                         (* (d' / fromIntegral 
                            (numAtEachDT `atUV` i))) sx
                   !de = UV.zipWith (+) accD $
                         UV.map (* (d' / n)) $
                         UV.zipWith (-) ss $ 
                         UV.map ((/n).(^^(2::Int))) sx  
               in (nu,de) 
             ) (UV.replicate nGene 0,UV.replicate nGene 0) ls

data PIIV = P {-# UNPACK #-} !Int
              {-# UNPACK #-} !Int
            !(UV.Vector FloatType)


coxPermTest :: Int -> Seed -> ExpData -> SurvivalTime -> FailureVec -> (PValues,Seed)
coxPermTest !n !rand_seed !chipData !survivalT !failVec =
  assert (V.length chipData == UV.length survivalT && 
          V.length chipData == UV.length failVec) $! 
  (pValues,r_s)
  where
    nSample = UV.length survivalT
    nFeature = UV.length $ chipData `V.unsafeIndex` 0
    idxVec = UV.enumFromN 0 nSample
    (randIdxs,r_s) = foldl' (\(acc,seed) _ ->
                         let !(v,s) = shuffle seed idxVec
                             !ls = v:acc
                         in (ls,s))
                ([],rand_seed) [0..n-1]
    !scores = coxScore chipData survivalT failVec
    !scoresFromPerm = V.fromList $ 
                      parMap rseq
                      (\randIdxVec ->
                        let !r_sur = UV.unsafeBackpermute 
                                     survivalT randIdxVec
                            !r_cen = UV.unsafeBackpermute 
                                     failVec randIdxVec
                        in coxScore chipData r_sur r_cen
                      ) randIdxs
    !pValues = UV.map
               ((/ fromIntegral (n + 1)) .
                (\(l,e) ->
                  mean $ UV.enumFromN (l + 1) (e + 1)
                )) $
               V.foldl'
               (\acc v ->
                 let !v' = UV.zipWith 
                           (\eP e ->
                             if abs eP > abs e
                             then (1,0)
                             else if abs eP == abs e
                                  then (0,1)
                                  else (0,0)
                           ) v scores
                 in UV.zipWith (\(v1,v2) (a1,a2) -> (v1+a1,v2+a2)) v' acc
               ) (UV.replicate nFeature (0,0)) scoresFromPerm

{-# INLINE shuffle #-}
{-# SPECIALIZE shuffle :: Seed -> UV.Vector Int -> (UV.Vector Int,Seed) #-}
{-# SPECIALIZE shuffle :: Seed -> V.Vector Int -> (V.Vector Int,Seed) #-}
{-# SPECIALIZE shuffle :: G.Vector v Int => Seed -> v Int -> (v Int,Seed) #-}
{-# SPECIALIZE shuffle :: G.Vector v Double => Seed -> v Double -> (v Double,Seed) #-}  
shuffle :: G.Vector v a => Seed -> v a -> (v a,Seed)
shuffle s v =
  runST $ do
    let len = G.length v
        n   = len-1
    mv <- GM.new len
    gen <- initialize $ fromSeed s
    G.unsafeCopy mv v
    forM_ [0..n] $ \idx -> do
      idx' <- uniformR (idx,n) gen
      val_i <- GM.read mv idx
      val_j <- GM.read mv idx'
      GM.write mv idx val_j
      GM.write mv idx' val_i
    s' <- save gen
    v' <- G.unsafeFreeze mv
    return $ (v',s')

