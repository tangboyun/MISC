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

module PageRank
       (
         pageRank
       )
       where

import Data.Graph
import Numeric.Container
import Numeric.LinearAlgebra.Util
import Data.Tuple
import Control.Monad.ST
import System.Random.MWC
import qualified Data.Vector.Storable as SV

dampingFactor :: Double
dampingFactor = 0.85

eps :: Double
eps = 1e-6

-- | http://en.wikipedia.org/wiki/PageRank
pageRank :: Ord key => [(key,[key])] -> [(key,Double)]
pageRank ps = let ls = toList $ go v last_v
                  s  = sum ls
              in zip (map fst ps) (map (/ s) ls)
  where
    ks = map fst ps
    (g,_,_) = graphFromEdges $ 
              map (\(a,b) -> 
                    let b' = if null b
                             then ks 
                             else b
                    in  (a,a,b')
                    ) ps
    l = length ps
    m' = accum (zeros l l) (+) $ 
         zip (map swap $ edges g) $ repeat 1.0
    colSum = fromList $ map sumElements $ toColumns m'
    m = fromRows $ map (`divide` colSum) $ toRows m' 
    r_v = fromList $ SV.toList $ runST $ do
      gen <- create
      uniformVector gen l
    v_n = norm r_v  
    v = cmap (/ v_n) r_v 
    last_v = fromList $ replicate l (1.0/0) -- vector of INF
    m_hat = scale dampingFactor m `add` 
            scale ((1 - dampingFactor) / fromIntegral l)
            (ones l l)
    go vec vec_ 
      | norm (vec `sub` vec_) > eps =
        let v' = m_hat <> vec 
            v_ = norm v'
        in go (cmap (/ v_) v') vec
      | otherwise = vec
    
-- | Logo on the wiki page.    
testGraph = 
  [("A",[])
  ,("B",["C"])
  ,("C",["B"])
  ,("D",["A","B"])
  ,("E",["B","D","F"])
  ,("F",["B","E"])
  ,("G",["B","E"])
  ,("H",["B","E"])
  ,("I",["B","E"])
  ,("J",["E"])
  ,("K",["E"])]
