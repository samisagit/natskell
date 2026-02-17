module Pipeline.Streaming.Source where
import           Conduit
import           Control.Concurrent (threadDelay)
import           Data.ByteString
import           Lib.Logger
import           Network.API
import           Prelude            hiding (length, null)

source :: (MonadLogger m , MonadIO m, ConnectionReader reader)
  => reader
  -> ConduitT () ByteString m ()
source reader = do
  lift . logMessage Debug $ "reading from connection"
  result <- liftIO $ readData reader 4096
  case result of
    Left err -> do
      lift . logMessage Error $ ("read failed: " ++ err)
    Right chunk -> do
      case null chunk of
        True -> do
          lift . logMessage Debug $ "no data read, waiting"
          liftIO $ threadDelay 100000
          source reader
        _ -> do
          lift . logMessage Debug $ ("read " ++ show (length chunk) ++ " bytes")
          yield chunk
          source reader
