{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Logger where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader

class MonadIO m => MonadLogger m where
  logDebug :: String -> m ()
  logInfo  :: String -> m ()
  logWarn  :: String -> m ()
  logError :: String -> m ()
  logFatal :: String -> m ()

data LogLevel = Debug | Info | Warn | Error | Fatal
  deriving (Eq, Ord, Show)

firstChar :: LogLevel -> Char
firstChar Debug = 'D'
firstChar Info  = 'I'
firstChar Warn  = 'W'
firstChar Error = 'E'
firstChar Fatal = 'F'

data LoggerConfig = LoggerConfig
                      { minLogLevel :: LogLevel
                      , logFn       :: LogLevel -> String -> IO ()
                      , logLock     :: TMVar ()
                      }

newtype AppM a = AppM { runAppM :: ReaderT LoggerConfig IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader LoggerConfig)

instance MonadLogger AppM where
  logDebug = logGeneric Debug
  logInfo  = logGeneric Info
  logWarn  = logGeneric Warn
  logError = logGeneric Error
  logFatal = logGeneric Fatal

logGeneric :: LogLevel -> String -> AppM ()
logGeneric lvl msg = do
  LoggerConfig minLvl out lock <- ask
  when (lvl >= minLvl) . liftIO $ bracket
      (atomically $ takeTMVar lock)
      (\() -> atomically $ putTMVar lock ())
      (\() -> out lvl msg)

runWithLogger :: LoggerConfig -> AppM a -> IO a
runWithLogger cfg = flip runReaderT cfg . runAppM

defaultLogger :: IO LoggerConfig
defaultLogger = do
  lock <- newTMVarIO ()
  pure $ LoggerConfig Debug logStdout lock
  where
    logStdout lvl msg = putStrLn $ "[" ++ [firstChar lvl] ++ "] " ++ msg

