{-# LANGUAGE ScopedTypeVariables #-}

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
  lift . logDebug $ "reading from socket"
  result <- liftIO $ readData reader 4096
  case result of
    Left err -> do
      lift . logError $ ("Error reading data: " ++ err)
    Right chunk -> do
      case null chunk of
        True -> do
          lift . logDebug $ "no data read, waiting"
          liftIO $ threadDelay 100000
          source reader
        _ -> do
          lift . logDebug $ ("read " ++ show (length chunk) ++ " bytes")
          yield chunk
          source reader

