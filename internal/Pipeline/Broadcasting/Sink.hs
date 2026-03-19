module Pipeline.Broadcasting.Sink where

import           Conduit
import qualified Data.ByteString.Lazy  as LBS
import           Lib.Logger.Types      (LogLevel (..), MonadLogger (..))
import           Network.ConnectionAPI (WriterAPI (..))

sink :: (MonadLogger m , MonadIO m)
  => WriterAPI writer
  -> writer
  -> ConduitT LBS.ByteString Void m ()
sink writerApi writer = do
  bs <- await
  case bs of
    Nothing -> do
      lift . logMessage Debug $ "no more data to write; stopping sink"
      return ()
    Just bs -> do
      res <- liftIO $ writerWriteDataLazy writerApi writer bs
      case res of
        Left err -> do
          lift . logMessage Error $ ("write failed: " ++ err)
          return ()
        Right () -> sink writerApi writer
