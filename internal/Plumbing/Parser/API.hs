module Parser.API
  ( ParserErr (..)
  , Parser (..)
  , Suggestion (..)
  , ParserAPI (..)
  ) where

import           Data.ByteString (ByteString)

data ParserErr = UnexpectedEndOfInput String Int
               | UnexpectedChar String Int
  deriving (Eq, Show)

newtype Parser a = Parser { runParser :: ByteString -> Either ParserErr (a, ByteString) }

data Suggestion = SuggestDrop Int String
                | SuggestPull
  deriving (Eq, Show)

-- | API wrapper for parser capabilities.
data ParserAPI a = ParserAPI
                     { parse :: ByteString -> Either ParserErr (a, ByteString)
                     , solveErr :: ParserErr -> Int -> Suggestion
                     }
