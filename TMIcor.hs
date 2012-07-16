{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference: A Permutation Approach to Testing Interactions in Many Dimensions
--            Noah Simon, Robert Tibshirani
-- 
--
-----------------------------------------------------------------------------

module TMIcor
       (
         tmiCor
       )
       where
import           Control.Parallel.Strategies
import           Data.Function
import           Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           GHC.Conc (numCapabilities)
import           Statistics.Basic
import           Statistics.Sampling
import           System.Random.MWC

type FloatType = Double
type FDR = FloatType
type Label = UV.Vector Bool
type ExpData = V.Vector (UV.Vector FloatType)

chunkSize :: Int
chunkSize = 1000

standization :: ExpData -> ExpData
standization !m =
  let (!vMean,!vVar) = vecFastMeanVar m
      !vStd = UV.map sqrt vVar
  in V.map (\e -> UV.zipWith (/) (UV.zipWith (-) e vMean) vStd) m

{-# INLINE (<.>) #-}
(<.>) :: UV.Vector FloatType -> UV.Vector FloatType -> FloatType
(<.>) !x1 !x2 = UV.foldl (+) 0 $ UV.zipWith (*) x1 x2

splitEvery :: Int -> [a] -> [[a]]
splitEvery n [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)

tmiCor_impl :: ExpData 
            -> Label   
            -> UV.Vector ((Int,Int),FloatType) -- ^ idx pairs & corr-change
tmiCor_impl !expData !labelVec =
  let m1 = V.unsafeBackpermute expData $
           UV.convert $ UV.findIndices id labelVec
      m2 = V.unsafeBackpermute expData $
           UV.convert $ UV.findIndices not labelVec
      p = UV.length $! m1 `V.unsafeIndex` 0
      canList =  [ (x,y) | y <- [0..p-1], x <- [0..y-1]]
      corM = concat $ parMap (parBuffer numCapabilities rdeepseq) 
             (map (\(i,j) ->
                    let r1 = (m1 `V.unsafeIndex` i) <.> (m1 `V.unsafeIndex` j)
                        r2 = (m2 `V.unsafeIndex` i) <.> (m2 `V.unsafeIndex` j)
                    in ((i,j),r1 - r2))
             ) $ splitEvery chunkSize canList  
  in UV.fromList $! sortBy (flip compare `on` (abs.snd)) corM


tmiCor :: Seed -- ^ initial random seed
       -> Int  -- ^ repeat times for permutation
       -> Int  -- ^ first k gene pairs
       -> ExpData -- ^ vector of gene chip
       -> Label   -- ^ binary label
       -> ((UV.Vector ((Int,Int),FloatType),FDR),Seed)
tmiCor seed nPerm firstK expData labelVec =
  let m1 = standization $!
           V.unsafeBackpermute expData $
           UV.convert $ UV.findIndices id labelVec
      m2 = standization $!
           V.unsafeBackpermute expData $
           UV.convert $ UV.findIndices not labelVec
      perIdx = V.fromList $
               map fst $ sortBy (compare `on` snd) $
               UV.toList $
               UV.indexed $
               (UV.findIndices id labelVec) UV.++
               (UV.findIndices not labelVec)
      expData' = V.unsafeBackpermute (m1 V.++ m2) perIdx
      vec = UV.take firstK $ tmiCor_impl expData' labelVec
      l = snd $ UV.last vec
      (fc,seed') = foldl' (\(acc,s) _ ->
                            let (label',s') = shuffle s labelVec
                                acc' = acc +
                                       (UV.length $
                                        UV.findIndices ((> l).abs.snd) $
                                        tmiCor_impl expData' label')
                            in (acc',s')
                          ) (0,seed) [1..nPerm]
      fdr = fromIntegral fc / fromIntegral nPerm / fromIntegral firstK
  in ((vec,fdr),seed')
