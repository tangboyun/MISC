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
module Styles where
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Builder
import           Data.Colour.Names

defaultS = emptyStyle { fontName = Just "Times New Roman"
                      , fontFamily = Just "Roman"
                      , fontSize = Just 10
                      }
boldCell = defaultS { fontIsBold = Just True }
title = boldCell { hAlign = Just "Center" }
upTitle = title { bgColor = Just red }
dnTitle = title { bgColor = Just green }
noteCellStyle = defaultS
  { bgColor = Just khaki
  , vAlign = Just "Center"
  , wrapText = Just True
  }
  
allHeadStyle = defaultS
  { bgColor = Just khaki
  , vAlign = Just "Top"
  , hAlign = Just "Left"
  , wrapText = Just True
  , fontSize = Just 12
  }
  
frCellStyle = title { bgColor = Just orange }
riCellStyle = title { bgColor = Just cornflowerblue }
niCellStyle = title { bgColor = Just lightgreen}
annoCellStyle = title { bgColor = Just lightpink }
whiteCellStyle = defaultS { bgColor = Just white }
groupRawCellStyle = title { bgColor = Just darkcyan }
groupNorCellStyle = title { bgColor = Just darkolivegreen }
