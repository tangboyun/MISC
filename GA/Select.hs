-----------------------------------------------------------------------------
-- |
-- Module : Selection operator for GA
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------
module GA.Select
       (
         rws -- ^ Roulette Wheel Selection
       , sus -- ^ Stochastic universal sampling
       , tns -- ^ Tournament selection
       )
       where
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.List
import           Data.Ord
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as UV
import           GA.Internal
import           GA.Types
import           Statistics.Sampling
import           System.Random.MWC

-- | Roulette Wheel Selection
-- http://en.wikipedia.org/wiki/Stochastic_universal_sampling
rws :: Int                          -- ^ Number of individual to be selected
    -> (Chrom a, UV.Vector Fitness) -- ^ Vector of fitness values
    -> State Seed (Chrom a,UV.Vector Fitness)
rws nSel (chrom,fVec) = 
  let cumsumV = UV.postscanl' (+) 0 fVec
      total   = UV.last cumsumV
  in state $ \seed -> 
  runST $ do
    gen   <- initialize $ fromSeed seed
    rV    <- uniformVector gen nSel 
    let idxs = 
          UV.map (\r -> 
                UV.length $ UV.takeWhile (< r) cumsumV) $ 
          UV.map (total *) rV
        chrom' = V.unsafeBackpermute chrom $ UV.convert idxs
        fVec'  = UV.unsafeBackpermute fVec idxs
    seed' <- save gen
    return ((chrom',fVec'),seed')
    
-- | Stochastic universal sampling
-- http://en.wikipedia.org/wiki/Stochastic_universal_sampling
sus :: Int                          -- ^ Number of individual to be selected
    -> (Chrom a,UV.Vector Fitness)  -- ^ Vector of fitness values
    -> State Seed (Chrom a,UV.Vector Fitness)
sus nSel (chrom,fVec) = 
  let cumsumV = UV.postscanl' (+) 0 fVec
      total   = UV.last cumsumV
  in state $ \seed ->
  runST $ do
    gen   <- initialize $ fromSeed seed
    r     <- uniform gen
    s <- save gen
    let trails = UV.map (\d -> 
                          (r + d) * total / 
                          fromIntegral nSel
                        ) $ UV.enumFromN 0 nSel
        is     = UV.map (\t -> 
                          UV.length $ UV.takeWhile (< t) cumsumV
                        ) trails
        (iV,seed') = shuffle s $ UV.enumFromN 0 (UV.length is)
        idxs   = UV.unsafeBackpermute is iV 
        chrom' = V.unsafeBackpermute chrom $ UV.convert idxs
        fVec'  = UV.unsafeBackpermute fVec idxs
    return ((chrom',fVec'),seed')

-- | Tournament selection
-- http://en.wikipedia.org/wiki/Tournament_selection
tns :: Int                          -- ^ Number of tournament(individual selected)
    -> Double                       -- ^ Ratio of (individual taking part in tour / total)
    -> (Chrom a,UV.Vector Fitness)  -- ^ Vector of fitness
    -> State Seed (Chrom a,UV.Vector Fitness)
tns nSel ratio (chrom,fVec) = 
  let 
    popSize     = UV.length fVec
    nTakePartIn = ceiling $ fromIntegral popSize * ratio 
    nInEachTour = ceiling $ fromIntegral nTakePartIn / 
                  fromIntegral nSel
  in state $ \seed ->
    let (iV,seed') = shuffle seed $ UV.enumFromN 0 popSize
        idxsShuffled = UV.toList iV
        idxs         = map (minimumBy 
                            (comparing (fVec `atUV`))
                            ) $ splitEvery nInEachTour $ 
                            take nTakePartIn idxsShuffled
        chrom' = V.unsafeBackpermute chrom $ V.fromList idxs
        fVec'  = UV.unsafeBackpermute fVec $ UV.fromList idxs
    in ((chrom',fVec'),seed')

