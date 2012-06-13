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

module LogRank
       where
import           Data.Function
import           Data.List
import qualified Data.Vector.Unboxed as UV
import           Statistics.Distribution
import           Statistics.Distribution.ChiSquared

type FloatType = Double


logRank :: [UV.Vector (FloatType,Bool)] -> FloatType
logRank groups =
  let nGroup = length groups
      actDeathsInEachGroup = UV.fromList $
                             map (fromIntegral . UV.length . UV.filter snd)  groups
      ls = sortBy (compare `on` fst) $ concatMap UV.toList groups
      totalSample = UV.fromList ls
      uniqueDT = UV.fromList $ nub $ map fst $ filter snd ls
      expDeathsInEachGroup =
        UV.foldl'
        (\accV deathTime ->
          let aliveInEachGroups =
                UV.fromList $
                map
                (fromIntegral . UV.length .
                 UV.findIndices ((>= deathTime) . fst)) groups
              nDeath = UV.length $
                       UV.findIndices (\e -> fst e == deathTime && snd e) totalSample
              alive = UV.foldl1' (+) aliveInEachGroups
              deathPro = fromIntegral nDeath / alive
              expDeathInEachG = UV.map (* deathPro) aliveInEachGroups
          in UV.zipWith (+) accV expDeathInEachG
          ) (UV.replicate nGroup 0) uniqueDT
      chiStatic = UV.foldl1' (+) $
                  UV.zipWith
                  (\a e ->
                    (a - e) * (a - e) / e
                  ) actDeathsInEachGroup expDeathsInEachGroup 
      distr = chiSquared (nGroup - 1)
  in complCumulative distr chiStatic
                 
