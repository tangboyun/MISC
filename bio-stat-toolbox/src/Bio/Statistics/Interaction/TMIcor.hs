{-# LANGUAGE BangPatterns,RankNTypes,FlexibleContexts,TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2013 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
-- Reference: A Permutation Approach to Testing Interactions in Many Dimensions
--            Noah Simon, Robert Tibshirani
-- 
--
-----------------------------------------------------------------------------

module Bio.Statistics.Interaction.TMIcor
       
       where
import           Bio.Statistics.Interaction.TMIcor.Internal
import           Control.DeepSeq
import           Control.Monad.Par.Combinator
import           Control.Monad.Par.Scheds.Direct
import           Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           System.Random.MWC



-- | Result: GeneIdx1, GeneIdx2, TMIcor-value, p-value, FDR
type Result = UV.Vector (Int,Int,Double,Double,Double)

data GData = GD
  { pVariable :: {-# UNPACK #-} !Int -- p >> n
  , nSample :: {-# UNPACK #-} !Int
  , dat :: !(UV.Vector Double)
  }

instance NFData GData where
  rnf (GD p n d) = rnf p `seq` rnf n `seq` rnf d
                      

tmiCor :: Seed -- ^ initial random seed
         -> Int  -- ^ repeat times for permutation
         -> Int  -- ^ first k gene pairs
         -> GData -- ^ vector of gene chip
         -> UV.Vector Bool -- ^ label for each sample
         -> (Result,Seed)
{-# INLINABLE tmiCor #-}       
tmiCor seed nPerm kPairs (GD p n _data) label' =
  let nTrue = count True label'
      vVec = V.fromList $ toStdScores nTrue $ toSlices n $ recomb n label' _data
      hs = kLargest nTrue kPairs vVec
      (gps,tCors) = unzip hs
      gpVec = UV.fromList gps
      tCorVec = UV.fromList tCors
      (ls,seed') = permute seed nPerm $ UV.enumFromN 0 n
      pVs = V.fromList ls
      minCor = abs $ head tCors
      psVec = getVec p
      t = p * (p-1) `quot` 2
      rVec = runPar $
             parMapReduceRangeThresh 1 (InclusiveRange 0 (nPerm - 1))
             (\pIdx ->
               let vVec' = V.fromList $
                           toStdScores nTrue $
                           toSlices n $
                           UV.concat $
                           map (flip UV.unsafeBackpermute (pVs `atV` pIdx)) $
                           V.toList vVec
                   (x,y) = if odd p
                           then (p,(p-1) `quot` 2)
                           else (p-1,p `quot` 2)
               in parMapReduceRangeThresh 1 (InclusiveRange 0 (x-1))
                  (\i ->
                    let is = take y [i*y..]
                        initIdx = head is
                        initI = fst $ calcIdx psVec initIdx
                    in return . UV.fromList .
                       countBy tCorVec .
                       dropWhile (<= minCor) . sort .
                       map ((\(a,b) ->
                              abs $
                              tCorStded nTrue
                              (vVec' `atV` a)
                              (vVec' `atV` b)
                            )) $ goIdx initI p $ is)
                  ((return .) . UV.zipWith (+))
                  (UV.replicate kPairs 0))
             ((return .) . UV.zipWith (+))
             (UV.replicate kPairs 0)
      csum = UV.postscanr' (+) 0 rVec      
      fdrVec = UV.imap
               (\i e ->
                 fromIntegral e /
                 fromIntegral ((kPairs-i)*nPerm)) csum
      pVec = UV.map ((/ fromIntegral (nPerm * t)) . fromIntegral) csum
      result = UV.reverse $
               UV.generate kPairs $
               (\k -> 
                 let (i,j) = gpVec `atUV` k
                     cor = tCorVec `atUV` k
                     fdr = fdrVec `atUV` k
                     pValue = pVec `atUV` k
                 in (i,j,cor,pValue,fdr))
  in (result,seed')

       


