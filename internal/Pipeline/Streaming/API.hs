{-# LANGUAGE RankNTypes #-}

module Pipeline.Streaming.API
  ( StreamingAPI (..)
  , Parser'
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString        (ByteString)
import           Lib.Logger.Types       (MonadLogger)
import           Lib.Parser.Types       (ParserErr)
import           Network.ConnectionAPI  (ReaderAPI)

type Parser' a = ByteString -> Either ParserErr (a, ByteString)

newtype StreamingAPI = StreamingAPI { streamingRun :: forall m reader a. (MonadLogger m, MonadIO m) => Int -> ReaderAPI reader -> reader -> Parser' a -> (a -> IO ()) -> m () }
