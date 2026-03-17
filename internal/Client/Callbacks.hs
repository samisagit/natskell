module Client.Callbacks
  ( enqueueCallback
  , startCallbackWorkers
  , awaitCallbackDrain
  ) where

import           Client.LifecycleAPI    (LifecycleAPI (lifecycleWaitForClosed))
import           Client.RuntimeAPI
    ( ClientState (..)
    , Config (callbackConcurrency)
    , RuntimeAPI (runtimeReadConfig, runtimeRunClient)
    )
import           Control.Concurrent.STM
    ( atomically
    , check
    , modifyTVar'
    , readTVar
    , writeTQueue
    )
import           Control.Exception      (displayException, finally)
import           Lib.Logger             (LogLevel (..), MonadLogger (..))
import           Lib.WorkerPool         (startWorkerPool)

enqueueCallback :: ClientState -> IO () -> IO ()
enqueueCallback client action = do
  let wrapped =
        action `finally` atomically (modifyTVar' (callbackPending client) (subtract 1))
  atomically $ do
    modifyTVar' (callbackPending client) (+1)
    writeTQueue (callbackQueue client) wrapped

startCallbackWorkers :: RuntimeAPI -> LifecycleAPI ClientState -> ClientState -> IO ()
startCallbackWorkers runtimeApi lifecycleApi client = do
  cfg <- runtimeReadConfig runtimeApi client
  let concurrency = max 1 (callbackConcurrency cfg)
      onError err =
        runtimeRunClient runtimeApi client . logMessage Error $
          "callback failed: " ++ displayException err
  startWorkerPool concurrency (callbackQueue client) (lifecycleWaitForClosed lifecycleApi client) onError

awaitCallbackDrain :: LifecycleAPI ClientState -> ClientState -> IO ()
awaitCallbackDrain lifecycleApi client = atomically $ do
  lifecycleWaitForClosed lifecycleApi client
  pending <- readTVar (callbackPending client)
  check (pending == 0)
