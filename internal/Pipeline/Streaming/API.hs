module Pipeline.Streaming.API where

import           Conduit
import           Data.ByteString
import           Lib.Logger
import           Lib.Parser
import           Network.API
import           Pipeline.Streaming.Parser
import           Pipeline.Streaming.Sink
import           Pipeline.Streaming.Source

type Parser' a = (ByteString -> Either ParserErr (a, ByteString))

run :: (MonadLogger m , MonadIO m, ConnectionReader reader)
  => reader
  -> Parser' a
  -> (a -> IO ())
  -> m ()
run reader p router = runConduit $ source reader .| parser p .| sink router
