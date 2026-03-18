module WorkerPoolSpec (spec) where

import           Control.Concurrent.STM
    ( atomically
    , check
    , modifyTVar'
    , newEmptyTMVarIO
    , newTQueueIO
    , newTVarIO
    , putTMVar
    , readTMVar
    , readTQueue
    , readTVar
    , readTVarIO
    , writeTQueue
    , writeTVar
    )
import           Control.Monad          (replicateM_, void, when)
import           Lib.WorkerPool         (startWorkerPool)
import           System.Timeout         (timeout)
import           Test.Hspec

spec :: Spec
spec = describe "WorkerPool" $ do
  it "limits concurrent jobs to the worker count" $ do
    queue <- newTQueueIO
    gate <- newTQueueIO
    stopVar <- newEmptyTMVarIO
    runningVar <- newTVarIO 0
    maxRunningVar <- newTVarIO 0

    let job = do
          atomically $ do
            running <- readTVar runningVar
            let running' = running + 1
            writeTVar runningVar running'
            maxRunning <- readTVar maxRunningVar
            when (running' > maxRunning) $
              writeTVar maxRunningVar running'
          atomically $ readTQueue gate
          atomically $ modifyTVar' runningVar (subtract 1)

    startWorkerPool 2 queue (void (readTMVar stopVar)) (const (pure ()))
    replicateM_ 4 (atomically $ writeTQueue queue job)

    started <- timeout 2000000 (atomically $ do
      maxRunning <- readTVar maxRunningVar
      check (maxRunning >= 2))
    started `shouldBe` Just ()

    replicateM_ 4 (atomically $ writeTQueue gate ())
    done <- timeout 2000000 (atomically $ do
      running <- readTVar runningVar
      check (running == 0))
    done `shouldBe` Just ()

    atomically $ putTMVar stopVar ()
    maxRunning <- readTVarIO maxRunningVar
    maxRunning `shouldBe` 2
