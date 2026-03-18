module Pipeline.Broadcasting
  ( run
  , broadcastingApi
  ) where

import           Conduit
import           Lib.Logger.Types                  (MonadLogger)
import           Network.API                       (ConnectionWriter)
import           Pipeline.Broadcasting.API         (BroadcastingAPI (..))
import           Pipeline.Broadcasting.Sink
import           Pipeline.Broadcasting.Source
import           Pipeline.Broadcasting.Transformer
import           Queue.API                         (Queue)
import           Transformers.Types                (Transformer)

run :: (MonadLogger m , MonadIO m, ConnectionWriter writer, Transformer t, Queue q t)
  => Int -> q -> writer -> m ()
run bufferLimit q w = do
  runConduit $ source q .| transformer bufferLimit .| sink w

broadcastingApi :: BroadcastingAPI
broadcastingApi = BroadcastingAPI
  { broadcastingRun = run
  }
