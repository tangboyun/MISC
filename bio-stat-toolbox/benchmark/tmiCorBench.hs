{-# LANGUAGE FlexibleContexts,BangPatterns #-}
import Criterion.Main
import System.Random.MWC
import Bio.Statistics.Interaction.TMIcor
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV

testData :: Int -> Int -> (GData UV.Vector,UV.Vector Bool,Seed)
testData n p =
  runST $ do
    gen <- create
    datV <- uniformVector gen (n*p) 
    bV <- uniformVector gen n
    seed <- save gen
    return (GD p n datV,bV,seed)
    
main :: IO ()
main = do
  let (gD,label,s) = testData 50 100
      re = fst $ tmiCorV1 s 100 100 gD label
      re2 = snd $ tmiCorV2 s 100 100 gD label
      re3 = snd $ tmiCorV3 s 100 100 gD label
  print $ and $ zipWith (==) re2 re3
  
  -- defaultMain
  --  [bench "tmiCor" $ whnf f (testData 100 10000)]
  -- where
  --   f = \(gD,label,s) -> tmiCor s 1000 2000 gD label
