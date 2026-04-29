module Lib.Logger
  ( module Lib.Logger.Types
  , defaultLogContext
  , newLogContext
  , updateLogContext
  , renderLogEntry
  , defaultLogger
  , withLogLock
  , loggerApi
  ) where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Data.Maybe             (catMaybes, fromMaybe)
import           Lib.Logger.Types
import           Lib.LoggerAPI          (LoggerAPI (LoggerAPI))

defaultLogContext :: LogContext
defaultLogContext = LogContext Nothing Nothing Nothing

newLogContext :: IO (TVar LogContext)
newLogContext = newTVarIO defaultLogContext

updateLogContext :: TVar LogContext -> (LogContext -> LogContext) -> IO ()
updateLogContext ctx f = atomically $ modifyTVar' ctx f

instance MonadWithLogger AppM where
  runWithLogger cfg ctx = flip runReaderT (LoggerEnv cfg ctx) . runAppM

instance MonadLogger AppM where
  logEntry entry = AppM . ReaderT $ \(LoggerEnv (LoggerConfig minLvl out lock) _) ->
    when (leLevel entry >= minLvl) (withLogLock lock (out entry))
  getLogContext = AppM . ReaderT $ \(LoggerEnv _ ctxVar) ->
    readTVarIO ctxVar
  logMessage lvl msg = AppM . ReaderT $ \(LoggerEnv (LoggerConfig minLvl out lock) ctxVar) ->
    when (lvl >= minLvl) $ do
      ctx <- readTVarIO ctxVar
      withLogLock lock $
        out LogEntry
          { leLevel = lvl
          , leMessage = msg
          , leClientId = lcClientId ctx
          , leConnectName = lcConnectName ctx
          , leServer = lcServer ctx
          }

renderLogEntry :: LogEntry -> String
renderLogEntry entry =
  let cidStr = maybe "unset" show (leClientId entry)
      cnStr  = fromMaybe "unset" (leConnectName entry)
      label  = "cid-" ++ cidStr ++ ":cn-" ++ cnStr
      ctxParts = catMaybes
        [ Just ("client=" ++ label)
        , fmap ("server=" ++) (leServer entry)
        ]
      ctxBlock = "[" ++ unwords ctxParts ++ "]"
  in "[" ++ levelTag (leLevel entry) ++ "] " ++ ctxBlock ++ " " ++ leMessage entry

levelTag :: LogLevel -> String
levelTag Debug = "D"
levelTag Info  = "I"
levelTag Warn  = "W"
levelTag Error = "E"
levelTag Fatal = "F"

defaultLogger :: IO LoggerConfig
defaultLogger = do
  lock <- newTMVarIO ()
  pure $ LoggerConfig Info (putStrLn . renderLogEntry) lock

withLogLock :: TMVar () -> IO a -> IO a
withLogLock lock =
  bracket_ (atomically $ takeTMVar lock) (atomically $ putTMVar lock ())

loggerApi :: LoggerAPI
loggerApi = LoggerAPI runWithLogger updateLogContext logMessage
