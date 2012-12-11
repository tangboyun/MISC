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
         minMax
       , mean
       , pcc
       , vecMean
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

minMax :: (Ord a, GV.Vector v a) => v a -> (a,a)
minMax v = (GV.minimum v, GV.maximum v)
{-# RULES
  "minMax/Unboxed.Vector Int" forall (v :: UV.Vector Int) . minMax v = minMaxI v;
  "minMax/Storable.Vector Int" forall (v :: SV.Vector Int) . minMax v = minMaxI v;
  "minMax/Unboxed.Vector Double" forall (v :: UV.Vector Double) . minMax v = minMaxF v;
  "minMax/Unboxed.Vector Float" forall (v :: UV.Vector Float) . minMax v = minMaxF v;
  "minMax/Storable.Vector Double" forall (v :: SV.Vector Double) . minMax v = minMaxF v;
  "minMax/Storable.Vector Float" forall (v :: SV.Vector Float) . minMax v = minMaxF v;
  #-}
{-# INLINABLE minMax #-}

minMaxI :: (Bounded a, Ord a, GV.Vector v a) => v a -> (a,a)
minMaxI v = if not $ GV.null v
            then GV.foldl' (\(minV,maxV) e ->
                             let minV' = if e < minV
                                         then e
                                         else minV
                                 maxV' = if e > maxV
                                         then e
                                         else maxV
                             in (minV',maxV')
                           ) (maxBound,minBound) v
            else error "Null Vector: minMax"
{-# INLINABLE minMaxI #-}           
  
minMaxF :: (Floating a, Ord a, GV.Vector v a) => v a -> (a,a)
minMaxF v = if not $ GV.null v
            then GV.foldl' (\(minV,maxV) e ->
                             let minV' = if e < minV
                                         then e
                                         else minV
                                 maxV' = if e > maxV
                                         then e
                                         else maxV
                             in (minV',maxV')
                           ) (1/0,-1/0) v
            else error "Null Vector: minMax"
{-# INLINABLE minMaxF #-}           

mean :: (Floating a, GV.Vector v a) => v a -> a
mean v = GV.ifoldl' (\acc i e -> acc + (e-acc) / fromIntegral (i+1)) 0 v
{-# INLINABLE mean #-}

-- | pearson correlation coef
pcc :: (Floating a, GV.Vector v a) => v a -> v a -> a
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
{-# INLINABLE pcc #-}
     
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
{-# INLINABLE vecMean #-}    

-- | Knuth's one-pass algorithm for computing sample variance.
-- /Note/: in cases where most sample data is close to the sample's
-- mean, Knuth's algorithm gives inaccurate results due to
-- catastrophic cancellation.
vecFastVar :: (Floating a,GV.Vector v a) => V.Vector (v a) -> v a
vecFastVar = snd . vecFastVarImpl


vecFastMeanVar :: (Floating a,GV.Vector v a) => V.Vector (v a) -> (v a,v a)
vecFastMeanVar = vecFastVarImpl

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
{-# INLINABLE vecMeanVar #-}     

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
{-# INLINABLE vecFastVarImpl #-}

