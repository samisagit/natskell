module Lib.WorkerPool
  ( startWorkerPool
  ) where

import           Control.Concurrent     (forkIO)
import           Control.Concurrent.STM
    ( STM
    , TQueue
    , atomically
    , check
    , isEmptyTQueue
    , orElse
    , readTQueue
    )
import           Control.Exception      (SomeException, try)
import           Control.Monad          (replicateM_, void)

startWorkerPool :: Int -> TQueue (IO ()) -> STM () -> (SomeException -> IO ()) -> IO ()
startWorkerPool concurrency queue stopSignal onError = do
  let workerCount = max 1 concurrency
  replicateM_ workerCount (void (forkIO worker))
  where
    worker = do
      let loop = do
            action <- atomically $
              (Just <$> readTQueue queue)
              `orElse`
              (do
                  stopSignal
                  empty <- isEmptyTQueue queue
                  check empty
                  return Nothing)
            case action of
              Nothing -> return ()
              Just job -> do
                result <- (try job :: IO (Either SomeException ()))
                case result of
                  Left err -> onError err
                  Right () -> pure ()
                loop
      loop
