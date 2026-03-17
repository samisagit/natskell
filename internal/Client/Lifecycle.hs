{-# LANGUAGE LambdaCase #-}

module Client.Lifecycle
  ( updateLogContextFromInfo
  , setServerInfo
  , setLifecycleClosing
  , markClosed
  , waitForClosed
  , waitForServerInfo
  , waitForClosing
  ) where

import           Client.LifecycleAPI
    ( ClientExitReason (..)
    , LifecycleState (..)
    )
import           Client.RuntimeAPI      (ClientState (..), ConfigState (..))
import           Control.Concurrent.STM
import           Lib.Logger.Types       (LogContext (lcClientId))
import           Lib.LoggerAPI          (LoggerAPI (loggerUpdateLogContext))
import qualified Types.Info             as I

updateLogContextFromInfo :: LoggerAPI -> ClientState -> I.Info -> IO ()
updateLogContextFromInfo loggerApi client info = do
  loggerUpdateLogContext loggerApi (logContext client) (\ctx -> ctx
    { lcClientId = I.client_id info
    })

setServerInfo :: ClientState -> I.Info -> STM ()
setServerInfo client info =
  modifyTVar' (configState client) (\state -> state { cfgServerInfo = Just info })

setLifecycleClosing :: ClientState -> ClientExitReason -> STM ()
setLifecycleClosing client reason =
  modifyTVar' (lifecycle client) $ \case
    Closed result -> Closed result
    Closing r     -> Closing r
    Running       -> Closing reason

markClosed :: ClientState -> ClientExitReason -> STM (Maybe ClientExitReason)
markClosed client fallbackReason = do
  state <- readTVar (lifecycle client)
  case state of
    Closed _ -> return Nothing
    Closing reason -> do
      writeTVar (lifecycle client) (Closed reason)
      return (Just reason)
    Running -> do
      writeTVar (lifecycle client) (Closed fallbackReason)
      return (Just fallbackReason)

waitForClosed :: ClientState -> STM ()
waitForClosed client = do
  state <- readTVar (lifecycle client)
  case state of
    Closed _ -> return ()
    _        -> retry

waitForServerInfo :: ClientState -> STM ()
waitForServerInfo client = do
  cfgState <- readTVar (configState client)
  case cfgServerInfo cfgState of
    Just _  -> return ()
    Nothing -> retry

waitForClosing :: ClientState -> STM ()
waitForClosing client = do
  state <- readTVar (lifecycle client)
  case state of
    Running -> retry
    _       -> return ()
