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
         shuffle
       , permute
       )
       where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import           System.Random.MWC


shuffle :: G.Vector v a => Seed -> v a -> (v a,Seed)
shuffle s v =
  runST $ do
    let len = G.length v
        n   = len-1
    mv <- GM.new len
    gen <- restore s
    G.unsafeCopy mv v
    forM_ [0..n] $ \idx -> do
      idx' <- uniformR (idx,n) gen
      GM.unsafeSwap mv idx idx'
    s' <- save gen
    v' <- G.unsafeFreeze mv
    return $ (v',s')
{-# INLINABLE shuffle #-}
    
permute :: G.Vector v a => Seed -> Int -> v a -> ([v a],Seed)
permute s n vec =
  let l = G.length vec
      v = G.concat $ replicate n vec
      t = n * l
      end = l - 1
  in runST $ do
    mv <- G.unsafeThaw v
    gen <- restore s
            
    forM_ [0..t-1] $ \idx -> do
      let (c,r) = idx `divMod` l
          i = c * l
      i' <- uniformR (r,end) gen
      GM.unsafeSwap mv idx (i+i') 

    v' <- G.unsafeFreeze mv
    s' <- save gen
    return (map ((\i -> G.unsafeSlice i l v').(*l)) [0..n-1] ,s')
{-# INLINABLE permute #-}
