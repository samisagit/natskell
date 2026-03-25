module Pipeline.Broadcasting
  ( run
  , broadcastingApi
  ) where

import           Conduit
import           Lib.Logger.Types                  (MonadLogger)
import           Network.ConnectionAPI             (WriterAPI)
import           Pipeline.Broadcasting.API         (BroadcastingAPI (..))
import           Pipeline.Broadcasting.Sink
import           Pipeline.Broadcasting.Source
import           Pipeline.Broadcasting.Transformer
import           Queue.API                         (Queue)

run :: (MonadLogger m , MonadIO m)
  => Int -> Queue -> WriterAPI writer -> writer -> m ()
run bufferLimit q writerApi writer = do
  runConduit $ source q .| transformer bufferLimit .| sink writerApi writer

broadcastingApi :: BroadcastingAPI
broadcastingApi = BroadcastingAPI
  { broadcastingRun = run
  }
