module Pipeline.Streaming
  ( run
  , streamingApi
  ) where

import           Conduit
import           Lib.Logger.Types          (MonadLogger)
import           Network.ConnectionAPI     (ReaderAPI)
import           Pipeline.Streaming.API    (Parser', StreamingAPI (..))
import           Pipeline.Streaming.Parser
import           Pipeline.Streaming.Sink
import           Pipeline.Streaming.Source

run :: (MonadLogger m , MonadIO m)
  => Int
  -> ReaderAPI reader
  -> reader
  -> Parser' a
  -> (a -> IO ())
  -> m ()
run bufferLimit readerApi reader p router =
  runConduit $ source readerApi reader .| parser bufferLimit p .| sink router

streamingApi :: StreamingAPI
streamingApi = StreamingAPI
  { streamingRun = run
  }
