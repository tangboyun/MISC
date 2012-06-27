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

module GSEA where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import Statistics.Sample
import Data.List
import System.Random.MWC
import Data.Function
import Control.Parallel.Strategies
import           Control.Monad
import           Control.Monad.ST

type FloatType = Double
type Label = UV.Vector FloatType -- Phono
type ExpData = V.Vector (UV.Vector FloatType)

enrich :: FloatType -> UV.Vector FloatType -> UV.Vector Int -> UV.Vector Int -> UV.Vector FloatType
enrich p score orderedList candidateSet =
  let cumulateFun = UV.foldl1' (+) . UV.map ((** p) . abs)
      nR = cumulateFun $! UV.unsafeBackpermute score candidateSet
      nMiss = fromIntegral $! UV.length orderedList - (UV.length candidateSet)
      canList = UV.toList candidateSet
      ords = UV.map (`elem` canList) orderedList
  in UV.imap (\idxInOrdList _ ->
               let !len = idxInOrdList + 1
                   !nHit = cumulateFun $ UV.unsafeBackpermute score $ UV.findIndices id $ UV.take len ords
                   !pHit = nHit / nR
                   !pMiss = fromIntegral (UV.length $ UV.findIndices not $ UV.take len ords) / nMiss
               in pHit - pMiss
             ) orderedList

enrich' :: FloatType -- ^ p for weighting
        -> UV.Vector FloatType -- ^ scores for each gene
        -> UV.Vector Int -- ^ gene set for enrichment analysis
        -> UV.Vector FloatType -- ^ enrichment score along ordered idxs
enrich' p score candidateSet =
  let cumulateFun = UV.foldl1' (+) . UV.map ((** p) . abs)
      nR = cumulateFun $! UV.unsafeBackpermute score candidateSet
      orderedList = UV.fromList $ sortBy (flip compare `on` (score `UV.unsafeIndex`)) [0..UV.length score - 1]
      nMiss = fromIntegral $! UV.length score - (UV.length candidateSet)
      canList = UV.toList candidateSet
      ords = UV.map (`elem` canList) orderedList
      pHits = UV.postscanl' (+) 0 $
              UV.imap (\i s ->
                        if ords `UV.unsafeIndex` i
                        then abs (score `UV.unsafeIndex` s) ** p / nR
                        else 0) orderedList
      pMiss = UV.postscanl' (+) 0 $
              UV.imap (\i s ->
                        if not $ ords `UV.unsafeIndex` i
                        then 1 / nMiss
                        else 0) orderedList
  in UV.zipWith (-) pHits pMiss


cor :: ExpData -> Label -> UV.Vector FloatType
cor expData label | V.length expData == UV.length label =
  let !m = vecMean expData
      !l = mean label
      !coefDen = sqrt $ UV.foldl1' (+) $ UV.map (\e -> (e - l) ^^ 2) label
      nSample = UV.length label
      nFeature = UV.length $ expData `V.unsafeIndex` 0
      (num,den) = foldl'
                  (\(accNum,accDen) idx ->
                    let vec = expData `V.unsafeIndex` idx
                        !v = UV.zipWith (-) vec m
                        !accNum' = UV.zipWith (+) accNum $
                                   UV.map (* (label `UV.unsafeIndex` idx - l)) v
                        !accDen' = UV.zipWith (+) accDen $
                                   UV.map (^^ (2 :: Int)) v
                    in (accNum',accDen')
                  ) (UV.replicate nFeature 0
                    ,UV.replicate nFeature 0) [0..nSample-1]
  in UV.zipWith (/) num $ UV.map ((* coefDen) . sqrt) den
                  | otherwise = error "No.Sample /= No.Label"

enrichWithPermTest :: Seed -- ^ init random seed
                   -> Int  -- ^ permutation times
                   -> FloatType -- ^ p for weighting
                   -> FloatType -- ^ confidence interval
                   -> UV.Vector Int -- ^ gene set for enrichment analysis
                   -> ExpData -- ^ gene expression data 
                   -> Label -- ^ response value
                   -> ((UV.Vector Int,UV.Vector FloatType,UV.Vector (FloatType,FloatType)),Seed) -- ordered idx, enrichment score , ci low and up bound
enrichWithPermTest seed n p ci candidSet expData label =
  let score = cor expData label
      es = enrich' p score candidSet
      orderedList = UV.fromList $
                    sortBy
                    (flip compare `on` (score `UV.unsafeIndex`)) $
                    [0..UV.length score - 1]
      (rs,seed') = foldl' 
                   (\(ls,s) _ ->
                     let (l,s') = shuffle s label
                         ls' = l:ls
                     in (ls',s')
                   ) ([],seed) $ replicate n (0::Int)
      randBound = UV.fromList $ 
                  map ((\ls -> (head ls,last ls)) . take n2 . drop n1 . sort) $ transpose $
                  parMap rseq 
                  (\randLabel ->
                    let !score' = cor expData randLabel
                    in UV.toList $ enrich' p score' candidSet 
                  ) rs
      n1 = ceiling $ (1 - ci) * 0.5 * fromIntegral n 
      n2 = n - n1
  in ((orderedList,es,randBound),seed')

-- | permute candidSet
enrichWithPermTest' :: Seed -- ^ init random seed
                    -> Int  -- ^ permutation times
                    -> FloatType -- ^ p for weighting
                    -> FloatType -- ^ n std from random mean
                    -> UV.Vector Int -- ^ gene set for enrichment analysis
                    -> ExpData -- ^ gene expression data 
                    -> Label -- ^ response value
                    -> ((UV.Vector Int,UV.Vector FloatType,UV.Vector (FloatType,FloatType)),Seed) -- ordered idx, enrichment score , ci low and up bound
enrichWithPermTest' seed n p ci candidSet expData label =
  let score = cor expData label
      es = enrich' p score candidSet
      nInCan = UV.length candidSet
      nFeature = UV.length $ expData `V.unsafeIndex` 0
      idxV = UV.enumFromN 0 nFeature
      orderedList = UV.fromList $
                    sortBy
                    (flip compare `on` (score `UV.unsafeIndex`)) $
                    [0..UV.length score - 1]
      (rs,seed') = foldl' 
                   (\(ls,s) _ ->
                     let (l,s') = shuffle s idxV
                         ls' = UV.take nInCan l:ls
                     in (ls',s')
                   ) ([],seed) $ replicate n (0::Int)
      randEs = map sort $ transpose $
               parMap rseq 
               (\randCan ->
                  UV.toList $ enrich' p score randCan 
               ) rs
      randBound = UV.fromList $ 
                  map ((\ls -> (head ls,last ls)) . take n2 . drop n1 ) $ randEs
      n1 = ceiling $ (1 - ci) * 0.5 * fromIntegral n 
      n2 = n - n1
  in ((orderedList,es,randBound),seed')

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

vecMean :: (Floating a,G.Vector v a) => V.Vector (v a) -> v a  
vecMean vv = 
  let n = V.length vv
      l = G.length $ vv `V.unsafeIndex` 0
  in runST $ do
    m_acc <- GM.replicate l 0
    forM_ [0..n-1] $ \sample_idx -> do
      let k = sample_idx + 1
          source_vec = vv `V.unsafeIndex` sample_idx
      forM_ [0..l-1] $ \i -> do
        m <- m_acc `GM.unsafeRead` i
        let v = source_vec `G.unsafeIndex` i
            d = v - m
            m' = m + d / fromIntegral k
        GM.unsafeWrite m_acc i m'
    G.unsafeFreeze m_acc
{-# INLINE vecMean #-}
{-# SPECIALIZE vecMean :: V.Vector (UV.Vector Float) -> UV.Vector Float #-}
{-# SPECIALIZE vecMean :: V.Vector (UV.Vector Double) -> UV.Vector Double #-}
{-# SPECIALIZE vecMean :: (G.Vector v Float) => V.Vector (v Float) -> v Float #-}
{-# SPECIALIZE vecMean :: (G.Vector v Double) => V.Vector (v Double) -> v Double #-}
