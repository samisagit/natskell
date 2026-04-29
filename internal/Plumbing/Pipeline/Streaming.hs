module Pipeline.Streaming
  ( streamingApi
  ) where

import           Conduit
import           Lib.Logger.Types          (MonadLogger)
import           Network.ConnectionAPI     (ReaderAPI)
import           Pipeline.Streaming.API    (Parser', StreamingAPI (..))
import           Pipeline.Streaming.Parser
import           Pipeline.Streaming.Sink
import           Pipeline.Streaming.Source

runStreaming :: (MonadLogger m , MonadIO m)
  => Int
  -> ReaderAPI reader
  -> reader
  -> Parser' a
  -> (a -> IO ())
  -> m ()
runStreaming bufferLimit readerApi reader p router =
  runConduit $ source readerApi reader .| parser bufferLimit p .| sink router

streamingApi :: StreamingAPI
streamingApi = StreamingAPI
  { run = runStreaming
  }
