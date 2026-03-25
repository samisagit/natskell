module Client.Runtime
  ( readConfigState
  , readConfig
  , runClient
  , writeToClientQueue
  ) where

import           Client.RuntimeAPI
    ( ClientState (..)
    , Config (..)
    , ConfigState (..)
    )
import           Control.Concurrent.STM (readTVarIO)
import           Lib.Logger             (AppM, LogLevel (..), MonadLogger (..))
import           Lib.LoggerAPI          (LoggerAPI (..))
import           Queue.API              (QueueItem, queueEnqueue)

readConfigState :: ClientState -> IO ConfigState
readConfigState client = readTVarIO (configState client)

readConfig :: ClientState -> IO Config
readConfig = fmap cfgConfig . readConfigState

runClient :: LoggerAPI -> ClientState -> AppM a -> IO a
runClient loggerApi client action = do
  cfg <- readConfig client
  loggerRunWithLogger loggerApi (loggerConfig cfg) (logContext client) action

writeToClientQueue :: LoggerAPI -> ClientState -> QueueItem -> IO ()
writeToClientQueue loggerApi client item = do
  res <- case client of
    ClientState {queue = q} -> queueEnqueue q item
  case res of
    Left err -> runClient loggerApi client . logMessage Error $ ("enqueueing item failed: " ++ err)
    Right () -> return ()
