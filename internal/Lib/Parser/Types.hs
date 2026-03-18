module Lib.Parser.Types
  ( ParserErr (..)
  , Parser (..)
  , Suggestion (..)
  ) where

import qualified Data.ByteString as BS

data ParserErr = UnexpectedEndOfInput String Int
               | UnexpectedChar String Int
  deriving (Eq, Show)

newtype Parser a = Parser { runParser :: BS.ByteString -> Either ParserErr (a, BS.ByteString) }

data Suggestion = SuggestDrop Int String
                | SuggestPull
