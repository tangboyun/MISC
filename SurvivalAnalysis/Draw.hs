{-# LANGUAGE BangPatterns #-}
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

module Draw where

import Data.List
import Data.Function
import Text.StringTemplate
import PGFPlotsTemplate

type FloatType = Double
type Point = (FloatType,FloatType)

calcPoints :: [(FloatType,Bool)] -> [(Point,Bool)]
calcPoints ls =
  let vs = sortBy (compare `on` fst) $ ls
      n   = length ls  
  in fst $ foldl' (\(ps,(nR,pLast)) (t,isFail) ->
                    let !nR' = nR - 1
                        !pLast' = if isFail
                                  then pLast * (fromIntegral nR' / 
                                                fromIntegral nR)
                                  else pLast
                        !ps' = ps ++ [((t,pLast'),isFail)]
                    in (ps',(nR',pLast'))
                  ) ([],(n,1.0)) vs


toTexString :: [(String,String)] -> [[(FloatType,Bool)]] -> String
toTexString paras args = 
  let curves = map (\((color,legend),ps) ->  
                     let points = calcPoints ps
                         as = (0,1):map fst points
                         cs = map fst $ filter (not . snd) points
                     in render $ 
                        setManyAttrib 
                        [("color",color)
                        ,("legend",legend)
                        ,("censoredPoints",intercalate "\n" $ map show cs)
                        ,("coordinates",intercalate "\n" $ map show as)
                        ] 
                        survivalCurveTemplate
                     ) $ zip paras args
  in render $ setAttribute "curves" curves texTemplate

