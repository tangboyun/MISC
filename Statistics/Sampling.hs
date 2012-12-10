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
       , shuffle
       )
       where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as UV
import           System.Random.MWC

{-# SPECIALIZE randomVecByFunc :: G.Vector v Double => Int -> Seed -> (forall s m . Gen s -> m Double) -> (v Double,Seed) #-}
{-# SPECIALIZE randomVecByFunc :: G.Vector v Float => Int -> Seed -> (forall s m . Gen s -> m Float) -> (v Float,Seed) #-}
{-# SPECIALIZE randomVecByFunc :: Int -> Seed -> (forall s m . Gen s -> m Double) -> (UV.Vector Double,Seed) #-}
{-# SPECIALIZE randomVecByFunc :: Int -> Seed -> (forall s m . Gen s -> m Float) -> (UV.Vector Float,Seed) #-}
randomVecByFunc :: (Floating a,G.Vector v a) => Int -> Seed -> (forall s m . Gen s -> m a) -> (v a,Seed)
randomVecByFunc len s f = runST $ do
  gen <- initialize $ fromSeed s
  vec <- GM.new len
  forM_ [0..len-1] $ \i -> do
     v <- f gen
     GM.unsafeWrite vec i v
  s' <- save gen
  vec' <- G.unsafeFreeze vec
  return (vec',s')

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

permute :: G.Vector v a => Int -> Seed -> v a -> ([v a],Seed)
permute n s vec =
  let l = G.length vec
      v = G.concat $ replicate n vec
      t = n * l
  in runST $ do
    mv <- G.unsafeThaw v
    gen <- restore s
    idxV <- UV.generateM t
            (\idx ->
              let (c,r) = idx `divMod` l
                  i = c * l
              in do
                i' <- uniformR (r, l - 1) gen
                return $! (i+i')
            )
            
    forM_ [0..t-1] $ \i -> 
      GM.unsafeSwap mv i (UV.unsafeIndex idxV i)
    v' <- G.unsafeFreeze mv
    s' <- save gen
    return (map ((\i -> G.unsafeSlice i l v').(*l)) [0..n-1] ,s')
