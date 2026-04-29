module Pipeline.Streaming
  ( streamingApi
  ) where

import           Conduit
import           Lib.Logger.Types          (MonadLogger)
import           Network.ConnectionAPI     (ReaderAPI)
import           Parser.API                (ParserAPI)
import           Pipeline.Streaming.API    (StreamingAPI (..))
import           Pipeline.Streaming.Parser
import           Pipeline.Streaming.Sink
import           Pipeline.Streaming.Source

runStreaming :: (MonadLogger m , MonadIO m)
  => Int
  -> ReaderAPI reader
  -> reader
  -> ParserAPI a
  -> (a -> IO ())
  -> m ()
runStreaming bufferLimit readerApi reader parserApi router =
  runConduit $ source readerApi reader .| parser bufferLimit parserApi .| sink router

streamingApi :: StreamingAPI
streamingApi = StreamingAPI
  { run = runStreaming
  }
