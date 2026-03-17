module Pipeline.Streaming
  ( run
  , streamingApi
  ) where

import           Conduit
import           Lib.Logger.Types          (MonadLogger)
import           Network.API               (ConnectionReader)
import           Pipeline.Streaming.API    (Parser', StreamingAPI (..))
import           Pipeline.Streaming.Parser
import           Pipeline.Streaming.Sink
import           Pipeline.Streaming.Source

run :: (MonadLogger m , MonadIO m, ConnectionReader reader)
  => Int
  -> reader
  -> Parser' a
  -> (a -> IO ())
  -> m ()
run bufferLimit reader p router =
  runConduit $ source reader .| parser bufferLimit p .| sink router

streamingApi :: StreamingAPI
streamingApi = StreamingAPI
  { streamingRun = run
  }
