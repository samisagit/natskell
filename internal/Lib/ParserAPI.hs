{-# LANGUAGE RankNTypes #-}

module Lib.ParserAPI
  ( ParserAPI (..)
  ) where

import           Data.ByteString  (ByteString)
import           Lib.Parser.Types

-- | API wrapper for parser capabilities.
data ParserAPI = ParserAPI
                   { parserRun :: forall a. Parser a -> ByteString -> Either ParserErr (a, ByteString)
                   , parserSolveErr :: ParserErr -> Int -> Suggestion
                   }
