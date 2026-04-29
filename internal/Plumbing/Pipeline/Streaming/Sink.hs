module Pipeline.Streaming.Sink where
import           Conduit
import           Lib.Logger.Types (LogLevel (..), MonadLogger (..))

sink :: (MonadIO m, MonadLogger m) => (a -> IO ()) -> ConduitT a Void m ()
sink action = do
  awaitForever $ \ma -> do
    liftIO $ action ma
    lift . logMessage Debug $ "executed action on message"
