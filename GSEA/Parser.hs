{-# LANGUAGE OverloadedStrings #-}
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

module Parser
       (
         readGCT
       , ExpData
       , GeneList
       , SampleList
       )
       where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B8
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Binary
import Data.List
import Control.Monad
import Data.Function
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

type FloatType = Double
type ExpData = V.Vector (UV.Vector FloatType)
type GeneList = V.Vector (ByteString,ByteString)
type SampleList = V.Vector ByteString

      
readGCT :: FilePath -> IO (ExpData,GeneList,SampleList)
readGCT fp = runResourceT $ sourceFile fp $$ sinkParser gctParser


gctParser :: Parser (ExpData,GeneList,SampleList)
gctParser = do
  _ <- string "#1.2"  *> manyTill anyChar (try endOfLine)
  nProbe <- decimal <* space
  nSample <- decimal <* manyTill anyChar (try endOfLine)
  ls <- many1 (satisfy (\e -> e /= '\t' && e /= '\n')) `sepBy1` char '\t' 
  let sV = V.fromList $ map B8.pack $ drop 2 ls
      parseGeneExp = do
        probe <- fmap B8.pack $ many1 (satisfy (/= '\t')) <* char '\t'
        symbl <- fmap B8.pack $ many1 (satisfy (/= '\t')) 
        expValue <- fmap UV.fromList $
                    count nSample (char '\t' *> double)
        return ((probe,symbl),expValue)
  (gs,es) <- fmap unzip $ count nProbe (endOfLine *> parseGeneExp)
  _ <- many space
  let gV = V.fromList gs
      totalV = UV.concat es
      expData = V.map
                (\sampleNo ->
                  UV.generate nProbe
                  (\probeNo ->
                    totalV `UV.unsafeIndex`
                    (sampleNo + probeNo * nSample)
                  )                     
                ) $ V.enumFromN 0 nSample
  return (expData,gV,sV)
  
