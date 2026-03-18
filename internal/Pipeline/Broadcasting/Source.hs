module Pipeline.Broadcasting.Source where
import           Conduit
import           Lib.Logger.Types   (LogLevel (..), MonadLogger (..))
import           Queue.API
import           Transformers.Types (Transformer)

source :: (MonadLogger m , MonadIO m, Transformer t, Queue q t)
  => q
  -> ConduitT () t m ()
source q  = do
  x <- liftIO . dequeue $ q
  case x of
    Left err -> do
      lift . logMessage Info $ ("dequeue failed: " ++ err)
    Right i -> do
      yield i
      source q
