{-# LANGUAGE RankNTypes #-}

module Lib.LoggerAPI
  ( LoggerAPI (..)
  ) where

import           Control.Concurrent.STM (TVar)
import           Lib.Logger.Types

-- | Thin API wrapper for logger capabilities.
data LoggerAPI = LoggerAPI
                   { runWithLogger :: forall a. LoggerConfig -> TVar LogContext -> AppM a -> IO a
                   , updateLogContext :: TVar LogContext -> (LogContext -> LogContext) -> IO ()
                   , logMessage :: LogLevel -> String -> AppM ()
                   }
