module Pipeline.Streaming.Sink where
import           Conduit
import           Control.Concurrent
import           Control.Monad
import           Lib.Logger

sink :: (MonadIO m, MonadLogger m) => (a -> IO ()) -> ConduitT a Void m ()
sink action = do
  awaitForever $ \ma -> do
    liftIO . void . forkIO $ action ma
    lift . logDebug $ "executed action on message"
    sink action
