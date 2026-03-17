{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Logger.Types
  ( MonadLogger (..)
  , MonadWithLogger (..)
  , LogLevel (..)
  , LoggerConfig (..)
  , LogContext (..)
  , LoggerEnv (..)
  , AppM (..)
  , LogEntry (..)
  ) where

import           Control.Concurrent.STM (TMVar, TVar)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT)

class MonadIO m => MonadLogger m where
  logEntry      :: LogEntry -> m ()
  getLogContext :: m LogContext
  logMessage    :: LogLevel -> String -> m ()

class (MonadLogger m, MonadIO m) => MonadWithLogger m where
  runWithLogger :: LoggerConfig -> TVar LogContext -> m a -> IO a

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

data LoggerEnv = LoggerEnv
                   { envConfig  :: LoggerConfig
                   , envContext :: TVar LogContext
                   }

newtype AppM a = AppM { runAppM :: ReaderT LoggerEnv IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader LoggerEnv)

data LogEntry = LogEntry
                  { leLevel       :: LogLevel
                  , leMessage     :: String
                  , leClientId    :: Maybe Int
                  , leConnectName :: Maybe String
                  , leServer      :: Maybe String
                  }
  deriving (Eq, Show)
