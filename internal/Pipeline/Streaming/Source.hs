module Pipeline.Streaming.Source where
import           Conduit
import           Data.ByteString
import           Lib.Logger.Types      (LogLevel (..), MonadLogger (..))
import           Network.ConnectionAPI (ReaderAPI (..))
import           Prelude               hiding (length, null)

source :: (MonadLogger m , MonadIO m)
  => ReaderAPI reader
  -> reader
  -> ConduitT () ByteString m ()
source readerApi reader = do
  lift . logMessage Debug $ "reading from connection"
  result <- liftIO $ readerReadData readerApi reader 4096
  case result of
    Left err -> do
      lift . logMessage Info $ ("read failed: " ++ err)
    Right chunk -> do
      case null chunk of
        True -> do
          lift . logMessage Info $ "read returned empty chunk; treating as disconnect"
          return ()
        _ -> do
          lift . logMessage Debug $ ("read " ++ show (length chunk) ++ " bytes")
          yield chunk
          source readerApi reader
