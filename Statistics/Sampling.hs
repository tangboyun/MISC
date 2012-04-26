{-# LANGUAGE Rank2Types,FlexibleContexts #-}
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
module Statistics.Sampling 
       (
         randomVecByFunc
       )
       where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic.Mutable as GM
import System.Random.MWC
import Control.Monad.ST
import Control.Monad

{-# INLINE randomVecByFunc #-}
{-# SPECIALIZE randomVecByFunc :: G.Vector v Double => Int -> Seed -> (forall s m . Gen s -> m Double) -> (v Double,Seed) #-}
{-# SPECIALIZE randomVecByFunc :: G.Vector v Float => Int -> Seed -> (forall s m . Gen s -> m Float) -> (v Float,Seed) #-}
{-# SPECIALIZE randomVecByFunc :: Int -> Seed -> (forall s m . Gen s -> m Double) -> (UV.Vector Double,Seed) #-}
{-# SPECIALIZE randomVecByFunc :: Int -> Seed -> (forall s m . Gen s -> m Float) -> (UV.Vector Float,Seed) #-}
randomVecByFunc :: (RealFloat a,G.Vector v a) => Int -> Seed -> (forall s m . Gen s -> m a) -> (v a,Seed)
randomVecByFunc len s f = runST $ do
  gen <- initialize $ fromSeed s
  vec <- GM.new len
  forM_ [0..len-1] $ \i -> do
     v <- f gen
     GM.unsafeWrite vec i v
  s' <- save gen
  vec' <- G.unsafeFreeze vec
  return (vec',s')
