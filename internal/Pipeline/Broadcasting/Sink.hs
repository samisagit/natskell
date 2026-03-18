module Pipeline.Broadcasting.Sink where

import           Conduit
import qualified Data.ByteString.Lazy as LBS
import           Lib.Logger.Types     (LogLevel (..), MonadLogger (..))
import           Network.API

sink :: (MonadLogger m , MonadIO m, ConnectionWriter writer)
  => writer
  -> ConduitT LBS.ByteString Void m ()
sink w = do
  bs <- await
  case bs of
    Nothing -> do
      lift . logMessage Debug $ "no more data to write; stopping sink"
      return ()
    Just bs -> do
      res <- liftIO $ writeDataLazy w bs
      case res of
        Left err -> do
          lift . logMessage Error $ ("write failed: " ++ err)
          return ()
        Right () -> sink w
