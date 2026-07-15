module Lib.WorkerPool
  ( startWorkerPool
  ) where

import           Control.Concurrent     (ThreadId, forkIOWithUnmask)
import           Control.Concurrent.STM
    ( STM
    , TQueue
    , atomically
    , check
    , isEmptyTQueue
    , orElse
    , readTQueue
    )
import           Control.Exception
    ( SomeAsyncException
    , SomeException
    , fromException
    , throwIO
    , try
    )
import           Control.Monad          (replicateM)

startWorkerPool :: Int -> TQueue (IO ()) -> STM () -> (SomeException -> IO ()) -> IO [ThreadId]
startWorkerPool concurrency queue stopSignal onError = do
  let workerCount = max 1 concurrency
  replicateM workerCount (forkIOWithUnmask (\unmask -> unmask worker))
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
                  Left err ->
                    case fromException err :: Maybe SomeAsyncException of
                      Just _  -> throwIO err
                      Nothing -> onError err
                  Right () -> pure ()
                loop
      loop
