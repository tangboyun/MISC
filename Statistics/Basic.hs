{-# LANGUAGE FlexibleContexts #-}
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
module Statistics.Basic 
       (
         vecMean
       , vecFastVar
       , vecFastMeanVar
       )
       where

import           Control.Monad
import           Control.Monad.ST.Strict
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Generic.Mutable as GVM
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Storable as SV

mean :: (Floating a, GV.Vector v a) => v a -> a
mean = GV.ifoldl' (\acc i e -> acc + (e-acc) / fromIntegral (i+1)) 0 
{-# SPECIALIZE mean :: UV.Vector Float -> Float #-}
{-# SPECIALIZE mean :: SV.Vector Float -> Float #-}
{-# SPECIALIZE mean :: UV.Vector Double -> Double #-}
{-# SPECIALIZE mean :: SV.Vector Double -> Double #-}
{-# SPECIALIZE mean :: (GV.Vector v Float) => v Float -> Float #-}
{-# SPECIALIZE mean :: (GV.Vector v Double) => v Double -> Double #-}

-- | pearson correlation coef
pcc :: (Real a,Floating a, GV.Vector v a) => v a -> v a -> a
pcc v1 v2 =
  let m1 = mean v1
      m2 = mean v2
  in (\(num,(res1,res2)) ->
       num / (sqrt $ res1 * res2)
       ) $ GV.ifoldl'
     (\(num,(res1,res2)) i e1 ->
       let val1 = e1
           val2 = GV.unsafeIndex v2 i
           num' = num + (val1-m1) * (val2-m2)
           res1' = res1 + (val1-m1)^2
           res2' = res2 + (val2-m2)^2
       in (num',(res1',res2'))
     ) (0,(0,0)) v1
{-# SPECIALIZE pcc :: UV.Vector Float -> UV.Vector Float -> Float #-}
{-# SPECIALIZE pcc :: SV.Vector Float -> SV.Vector Float -> Float #-}
{-# SPECIALIZE pcc :: UV.Vector Double -> UV.Vector Double -> Double #-}
{-# SPECIALIZE pcc :: SV.Vector Double -> SV.Vector Double -> Double #-}
{-# SPECIALIZE pcc :: (GV.Vector v Float) => v Float -> v Float -> Float #-}
{-# SPECIALIZE pcc :: (GV.Vector v Double) => v Double -> v Double -> Double #-}
     
vecMean :: (Floating a,GV.Vector v a) => V.Vector (v a) -> v a  
vecMean vv = 
  let n = V.length vv
      l = GV.length $ vv `V.unsafeIndex` 0
  in runST $ do
    m_acc <- GVM.replicate l 0
    forM_ [0..n-1] $ \sample_idx -> do
      let k = sample_idx + 1
          source_vec = vv `V.unsafeIndex` sample_idx
      forM_ [0..l-1] $ \i -> do
        m <- m_acc `GVM.unsafeRead` i
        let v = source_vec `GV.unsafeIndex` i
            d = v - m
            m' = m + d / fromIntegral k
        GVM.unsafeWrite m_acc i m'
    GV.unsafeFreeze m_acc
{-# SPECIALIZE vecMean :: V.Vector (UV.Vector Float) -> UV.Vector Float #-}
{-# SPECIALIZE vecMean :: V.Vector (UV.Vector Double) -> UV.Vector Double #-}
{-# SPECIALIZE vecMean :: (GV.Vector v Float) => V.Vector (v Float) -> v Float #-}
{-# SPECIALIZE vecMean :: (GV.Vector v Double) => V.Vector (v Double) -> v Double #-}

-- | Knuth's one-pass algorithm for computing sample variance.
-- /Note/: in cases where most sample data is close to the sample's
-- mean, Knuth's algorithm gives inaccurate results due to
-- catastrophic cancellation.
vecFastVar :: (Floating a,GV.Vector v a) => V.Vector (v a) -> v a
vecFastVar = snd . vecFastVarImpl
{-# INLINE vecFastVar #-}


vecFastMeanVar :: (Floating a,GV.Vector v a) => V.Vector (v a) -> (v a,v a)
vecFastMeanVar = vecFastVarImpl
{-# INLINE vecFastMeanVar #-}

vecMeanVar :: (Floating a,GV.Vector v a) => V.Vector (v a) -> (v a,v a)
vecMeanVar vv =
  let mv = vecMean vv
      nFeature = GV.length $ vv `V.unsafeIndex` 0
      nSample = fromIntegral $ V.length vv
      rss = V.foldl' (\acc v ->
                       GV.zipWith (+) acc $ GV.map (^^2) $ GV.zipWith (-) v mv
                       ) (GV.replicate nFeature 0) vv
      var = GV.map (/ (nSample - 1)) rss
  in (mv,var)

vecFastVarImpl :: (Floating a,GV.Vector v a) => V.Vector (v a) -> (v a,v a)
vecFastVarImpl vv = 
  let n = V.length vv
      l = GV.length $ vv `V.unsafeIndex` 0
  in runST $ do
    m_acc <- GVM.replicate l 0
    v_acc <- GVM.replicate l 0
    
    forM_ [0..n-1] $ \sample_idx -> do
      let k = sample_idx + 1
          source_vec = vv `V.unsafeIndex` sample_idx
      forM_ [0..l-1] $ \i -> do
        m <- m_acc `GVM.unsafeRead` i
        s <- v_acc `GVM.unsafeRead` i
        let v = source_vec `GV.unsafeIndex` i
            m' = m + d / fromIntegral k
            d = v - m
            s' = s + d * (v - m')
        GVM.unsafeWrite m_acc i m'
        GVM.unsafeWrite v_acc i s'
        
    forM_ [0..l-1] $ \i -> do
      v <- v_acc `GVM.unsafeRead` i
      GVM.unsafeWrite v_acc i $ v / fromIntegral (n-1)
    
    mvec <- GV.unsafeFreeze m_acc 
    vvec <- GV.unsafeFreeze v_acc
    return (mvec,vvec)
{-# SPECIALIZE vecFastVarImpl :: V.Vector (UV.Vector Float) -> (UV.Vector Float,UV.Vector Float) #-}
{-# SPECIALIZE vecFastVarImpl :: V.Vector (UV.Vector Double) -> (UV.Vector Double,UV.Vector Double) #-}
{-# SPECIALIZE vecFastVarImpl :: (GV.Vector v Float) => V.Vector (v Float) -> (v Float,v Float) #-}
{-# SPECIALIZE vecFastVarImpl :: (GV.Vector v Double) => V.Vector (v Double) -> (v Double,v Double) #-}


