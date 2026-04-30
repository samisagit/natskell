{-# LANGUAGE RankNTypes #-}

module Pipeline.Streaming.API
  ( StreamingAPI (..)
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Lib.Logger.Types       (MonadLogger)
import           Network.ConnectionAPI  (ReaderAPI)
import           Parser.API             (ParserAPI)

newtype StreamingAPI = StreamingAPI { run :: forall m reader a. (MonadLogger m, MonadIO m) => Int -> ReaderAPI reader -> reader -> ParserAPI a -> (a -> IO ()) -> m () }
