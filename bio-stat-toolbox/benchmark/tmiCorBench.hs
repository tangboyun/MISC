{-# LANGUAGE FlexibleContexts,BangPatterns #-}
import Criterion.Main
import System.Random.MWC
import Bio.Statistics.Interaction.TMIcor
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as UV

testData :: Int -> Int -> (GData,UV.Vector Bool,Seed)
testData n p =
  runST $ do
    gen <- create
    datV <- uniformVector gen (n*p) 
    bV <- uniformVector gen n
    seed <- save gen
    return (GD p n datV,bV,seed)
    
main :: IO ()
main = do
  let dat = testData 50 100
      v1 = f dat
      
--  print $ and $ map ((\v -> UV.and $ UV.zipWith (==) v1 v) .f) (replicate 10000 dat)
  defaultMain
     [bench "tmiCor" $ nf f dat ]
 where
   f = \(gD,label,s) ->
     fst $ tmiCor s 100 100 gD label
