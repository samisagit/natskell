{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Logger where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Maybe             (catMaybes, fromMaybe)

class MonadIO m => MonadLogger m where
  logEntry      :: LogEntry -> m ()
  getLogContext :: m LogContext
  logMessage    :: LogLevel -> String -> m ()
  logMessage lvl msg = do
    ctx <- getLogContext
    logEntry LogEntry
      { leLevel = lvl
      , leMessage = msg
      , leClientId = lcClientId ctx
      , leConnectName = lcConnectName ctx
      , leServer = lcServer ctx
      }

data LogLevel = Debug | Info | Warn | Error | Fatal
  deriving (Eq, Ord, Show)

data LoggerConfig = LoggerConfig
                      { minLogLevel :: LogLevel
                      , logFn       :: LogEntry -> IO ()
                      , logLock     :: TMVar ()
                      }

data LogContext = LogContext
                    { lcClientId    :: Maybe Int
                    , lcConnectName :: Maybe String
                    , lcServer      :: Maybe String
                    }
  deriving (Eq, Show)

defaultLogContext :: LogContext
defaultLogContext = LogContext Nothing Nothing Nothing

newLogContext :: IO (TVar LogContext)
newLogContext = newTVarIO defaultLogContext

updateLogContext :: TVar LogContext -> (LogContext -> LogContext) -> IO ()
updateLogContext ctx f = atomically $ modifyTVar' ctx f

data LoggerEnv = LoggerEnv
                   { envConfig  :: LoggerConfig
                   , envContext :: TVar LogContext
                   }

newtype AppM a = AppM { runAppM :: ReaderT LoggerEnv IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader LoggerEnv)

class (MonadLogger m, MonadIO m) => MonadWithLogger m where
  runWithLogger :: LoggerConfig -> TVar LogContext -> m a -> IO a

instance MonadWithLogger AppM where
  runWithLogger cfg ctx = flip runReaderT (LoggerEnv cfg ctx) . runAppM

data LogEntry = LogEntry
                  { leLevel       :: LogLevel
                  , leMessage     :: String
                  , leClientId    :: Maybe Int
                  , leConnectName :: Maybe String
                  , leServer      :: Maybe String
                  }
  deriving (Eq, Show)

instance MonadLogger AppM where
  logEntry entry = AppM . ReaderT $ \(LoggerEnv (LoggerConfig minLvl out lock) _) ->
    when (leLevel entry >= minLvl) (withLogLock lock (out entry))
  getLogContext = AppM . ReaderT $ \(LoggerEnv _ ctxVar) ->
    readTVarIO ctxVar

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
  pure $ LoggerConfig Debug (putStrLn . renderLogEntry) lock

withLogLock :: TMVar () -> IO a -> IO a
withLogLock lock =
  bracket_ (atomically $ takeTMVar lock) (atomically $ putTMVar lock ())
