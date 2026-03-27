module Parser.API
  ( ParserAPI (..)
  ) where

import           Data.ByteString (ByteString)
import           Parser.Types

-- | API wrapper for parser capabilities.
data ParserAPI a = ParserAPI
                     { parserParse :: ByteString -> Either ParserErr (a, ByteString)
                     , parserSolveErr :: ParserErr -> Int -> Suggestion
                     }
