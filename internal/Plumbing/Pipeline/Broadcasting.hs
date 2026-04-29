module Pipeline.Broadcasting
  ( broadcastingApi
  ) where

import           Conduit
import           Lib.Logger.Types                  (MonadLogger)
import           Network.ConnectionAPI             (WriterAPI)
import           Pipeline.Broadcasting.API         (BroadcastingAPI (..))
import           Pipeline.Broadcasting.Sink
import           Pipeline.Broadcasting.Source
import           Pipeline.Broadcasting.Transformer
import           Queue.API                         (Queue)

runBroadcasting :: (MonadLogger m , MonadIO m)
  => Int -> Queue -> WriterAPI writer -> writer -> m ()
runBroadcasting bufferLimit q writerApi writer = do
  runConduit $ source q .| transformer bufferLimit .| sink writerApi writer

broadcastingApi :: BroadcastingAPI
broadcastingApi = BroadcastingAPI
  { run = runBroadcasting
  }
