{-# LANGUAGE RankNTypes #-}

module Lib.LoggerAPI
  ( LoggerAPI (..)
  ) where

import           Control.Concurrent.STM (TVar)
import           Lib.Logger.Types

-- | Thin API wrapper for logger capabilities.
data LoggerAPI = LoggerAPI
                   { loggerRunWithLogger :: forall a. LoggerConfig -> TVar LogContext -> AppM a -> IO a
                   , loggerUpdateLogContext :: TVar LogContext -> (LogContext -> LogContext) -> IO ()
                   , loggerLogMessage :: LogLevel -> String -> AppM ()
                   }
