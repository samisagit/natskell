module Pipeline.Broadcasting.Source where
import           Conduit
import           Lib.Logger.Types (LogLevel (..), MonadLogger (..))
import           Queue.API        (Queue, QueueItem, dequeue)

source :: (MonadLogger m , MonadIO m)
  => Queue
  -> ConduitT () QueueItem m ()
source q  = do
  x <- liftIO (dequeue q)
  case x of
    Left err -> do
      lift . logMessage Info $ ("dequeue failed: " ++ err)
    Right i -> do
      yield i
      source q
