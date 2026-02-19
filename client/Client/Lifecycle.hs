module Client.Lifecycle
  ( updateLogContextFromInfo
  , setServerInfo
  , setLifecycleClosing
  , markClosed
  , waitForClosed
  , waitForServerInfo
  , waitForClosing
  ) where

import           Client.Types
import           Control.Concurrent.STM
import           Lib.Logger             (updateLogContext)
import           Options                (ClientExitReason (..))
import qualified Types.Info             as I

updateLogContextFromInfo :: Client -> I.Info -> IO ()
updateLogContextFromInfo client info = do
  updateLogContext (logContext client) (\ctx -> ctx
    { lcClientId = I.client_id info
    })

setServerInfo :: Client -> I.Info -> STM ()
setServerInfo client info =
  modifyTVar' (configState client) (\state -> state { cfgServerInfo = Just info })

setLifecycleClosing :: Client -> ClientExitReason -> STM ()
setLifecycleClosing client reason =
  modifyTVar' (lifecycle client) $ \state ->
    case state of
      Closed result -> Closed result
      Closing r     -> Closing r
      Running       -> Closing reason

markClosed :: Client -> ClientExitReason -> STM (Maybe ClientExitReason)
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

waitForClosed :: Client -> STM ()
waitForClosed client = do
  state <- readTVar (lifecycle client)
  case state of
    Closed _ -> return ()
    _        -> retry

waitForServerInfo :: Client -> STM ()
waitForServerInfo client = do
  cfgState <- readTVar (configState client)
  case cfgServerInfo cfgState of
    Just _  -> return ()
    Nothing -> retry

waitForClosing :: Client -> STM ()
waitForClosing client = do
  state <- readTVar (lifecycle client)
  case state of
    Running -> retry
    _       -> return ()
