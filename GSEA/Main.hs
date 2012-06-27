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

module Main where
import System.Environment
import Data.List.Split
import Data.List
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import System.Random.MWC
import GSEA
import GSEATemplate
import Debug.Trace
import Data.Function
import Parser
import Control.Monad

main :: IO ()
main = do
  expFile:lncFile:lncIdFile:pathWayFile:_ <- getArgs
  (expData,geneList,_) <- readGCT expFile
  lncStr <- readFile lncFile
  lncId <- fmap (head . words . head . lines) $ readFile lncIdFile
  candidList <- fmap (map ((\(name:_:gs) -> (name,gs)) .(map B8.pack . splitOn "\t")) . lines) $ readFile pathWayFile
  seed <- create >>= save
  let
      geneIds = V.toList $ fst $ V.unzip geneList
      geneMap = M.fromList $ zip geneIds [0..]
      lncExp = lncStr `seq` (V.fromList $ map UV.fromList $ filter (not . null) $ map (map read . tail . splitOn "\t") $ tail $ lines lncStr)
      lncIds = lncStr `seq` (map (B8.pack . head . splitOn "\t") $ tail $ lines lncStr)
      lncMap = M.fromList $ zip lncIds [0..]
      response = lncExp `V.unsafeIndex` (lncMap M.! (B8.pack lncId))
  forM_ candidList $ \(pathName,geneSet) -> do
    let candidSet = UV.fromList $ map (\e -> geneMap M.! e) (geneSet `intersect` geneIds)
        ((_,es,bounds),_) = enrichWithPermTest' seed 100 1 0.95 candidSet expData response
    writeFile (B8.unpack pathName ++ ".tex") $ toTexString (B8.unpack pathName) candidSet (es,bounds)
  
