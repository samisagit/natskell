module Pipeline.Broadcasting.Source where
import           Conduit
import           Lib.Logger
import           Queue.API
import           Transformers.Transformers (Transformer)

source :: (MonadLogger m , MonadIO m, Transformer t, Queue q t)
  => q
  -> ConduitT () t m ()
source q  = do
  x <- liftIO . dequeue $ q
  case x of
    Left err -> do
      lift . logMessage Error $ ("dequeue failed: " ++ err)
    Right i -> do
      yield i
      source q
