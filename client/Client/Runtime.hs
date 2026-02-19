module Client.Runtime
  ( readConfigState
  , readConfig
  , runClient
  , writeToClientQueue
  ) where

import           Client.Types
import           Control.Concurrent.STM   (readTVarIO)
import           Lib.Logger
import           Options                  (Config, loggerConfig)
import           Queue.API                (enqueue)
import           Queue.TransactionalQueue (QueueItem)

readConfigState :: Client -> IO ConfigState
readConfigState client = readTVarIO (configState client)

readConfig :: Client -> IO Config
readConfig = fmap cfgConfig . readConfigState

runClient :: Client -> AppM a -> IO a
runClient client action = do
  cfg <- readConfig client
  runWithLogger (loggerConfig cfg) (logContext client) action

writeToClientQueue :: Client -> QueueItem -> IO ()
writeToClientQueue client item = do
  res <- case client of
    Client' {queue = q} -> enqueue q item
  case res of
    Left err -> runClient client . logMessage Error $ ("enqueueing item failed: " ++ err)
    Right () -> return ()
