module Pipeline.Broadcasting.API where

import           Conduit
import           Lib.Logger                        (MonadLogger)
import           Network.API
import           Pipeline.Broadcasting.Sink
import           Pipeline.Broadcasting.Source
import           Pipeline.Broadcasting.Transformer
import           Queue.API
import           Transformers.Transformers         (Transformer)

run :: (MonadLogger m , MonadIO m, ConnectionWriter writer, Transformer t, Queue q t)
  => q -> writer -> m ()
run q w = do
  runConduit $ source q .| transformer .| sink w

