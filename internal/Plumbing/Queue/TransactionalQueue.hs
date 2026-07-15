module Queue.TransactionalQueue
  ( newQueue
  , newQueueWithCapacity
  ) where

import           Control.Concurrent.STM
import qualified Control.Monad
import           Data.List              (partition)
import           Numeric.Natural        (Natural)
import           Queue.API
    ( Queue (Queue)
    , QueueItem
    , TryEnqueueResult (..)
    , isConnectionScoped
    , isWithinPayloadLimit
    , payloadLimitRejections
    )

data TransactionalQueue = TransactionalQueue
                            { transactionalQueueItems  :: TBQueue QueueItem
                            , transactionalQueueClosed :: TMVar ()
                            }

newQueue :: IO Queue
newQueue = newQueueWithCapacity 1000

newQueueWithCapacity :: Natural -> IO Queue
newQueueWithCapacity capacity = do
  queueItems <- newTBQueueIO (max 1 capacity)
  queueClosed <- newEmptyTMVarIO
  let transactionalQueue =
        TransactionalQueue
          { transactionalQueueItems = queueItems
          , transactionalQueueClosed = queueClosed
          }
  pure $
    Queue
      (enqueue transactionalQueue)
      (enqueueSTM transactionalQueue)
      (tryEnqueue transactionalQueue)
      (tryEnqueueSTM transactionalQueue)
      (dequeue transactionalQueue)
      (close transactionalQueue)
      (open transactionalQueue)
      (openSTM transactionalQueue)
      (discardConnectionScoped transactionalQueue)
      (closeAndDiscardConnectionScoped transactionalQueue)
      (closeAndDiscardAll transactionalQueue)
      (discardOversizedPayloads transactionalQueue)

enqueue :: TransactionalQueue -> QueueItem -> IO (Either String ())
enqueue transactionalQueue = atomically . enqueueSTM transactionalQueue

enqueueSTM :: TransactionalQueue -> QueueItem -> STM (Either String ())
enqueueSTM transactionalQueue item =
  (do
    isOpen <- isEmptyTMVar (transactionalQueueClosed transactionalQueue)
    check isOpen
    writeTBQueue (transactionalQueueItems transactionalQueue) item
    pure (Right ()))
  `orElse`
  (Left "Queue is closed" <$ readTMVar (transactionalQueueClosed transactionalQueue))

tryEnqueue :: TransactionalQueue -> QueueItem -> IO TryEnqueueResult
tryEnqueue transactionalQueue = atomically . tryEnqueueSTM transactionalQueue

tryEnqueueSTM :: TransactionalQueue -> QueueItem -> STM TryEnqueueResult
tryEnqueueSTM transactionalQueue item = do
  isOpen <- isEmptyTMVar (transactionalQueueClosed transactionalQueue)
  if not isOpen
    then pure TryQueueClosed
    else do
      isFull <- isFullTBQueue (transactionalQueueItems transactionalQueue)
      if isFull
        then pure TryQueueFull
        else writeTBQueue (transactionalQueueItems transactionalQueue) item >> pure TryEnqueued

dequeue :: TransactionalQueue -> IO (Either String QueueItem)
dequeue transactionalQueue = atomically $ do
  isOpen <- isEmptyTMVar (transactionalQueueClosed transactionalQueue)
  if isOpen
    then Right <$> readTBQueue (transactionalQueueItems transactionalQueue)
    else pure (Left "Queue is closed")

close :: TransactionalQueue -> IO ()
close transactionalQueue = atomically $ do
  _ <- tryTakeTMVar (transactionalQueueClosed transactionalQueue)
  putTMVar (transactionalQueueClosed transactionalQueue) ()

open :: TransactionalQueue -> IO ()
open = atomically . openSTM

openSTM :: TransactionalQueue -> STM ()
openSTM transactionalQueue =
  Control.Monad.void (tryTakeTMVar (transactionalQueueClosed transactionalQueue))

discardConnectionScoped :: TransactionalQueue -> IO ()
discardConnectionScoped transactionalQueue = atomically $ do
  discardConnectionScopedSTM transactionalQueue

closeAndDiscardConnectionScoped :: TransactionalQueue -> STM ()
closeAndDiscardConnectionScoped transactionalQueue = do
  _ <- tryTakeTMVar (transactionalQueueClosed transactionalQueue)
  putTMVar (transactionalQueueClosed transactionalQueue) ()
  discardConnectionScopedSTM transactionalQueue

closeAndDiscardAll :: TransactionalQueue -> STM ()
closeAndDiscardAll transactionalQueue = do
  _ <- tryTakeTMVar (transactionalQueueClosed transactionalQueue)
  putTMVar (transactionalQueueClosed transactionalQueue) ()
  Control.Monad.void (flushTBQueue (transactionalQueueItems transactionalQueue))

discardConnectionScopedSTM :: TransactionalQueue -> STM ()
discardConnectionScopedSTM transactionalQueue = do
  queued <- flushTBQueue (transactionalQueueItems transactionalQueue)
  mapM_
    (writeTBQueue (transactionalQueueItems transactionalQueue))
    (filter (not . isConnectionScoped) queued)

discardOversizedPayloads :: TransactionalQueue -> Int -> STM [IO ()]
discardOversizedPayloads transactionalQueue maximumSize = do
  queued <- flushTBQueue (transactionalQueueItems transactionalQueue)
  let (retained, discarded) = partition (isWithinPayloadLimit maximumSize) queued
  mapM_ (writeTBQueue (transactionalQueueItems transactionalQueue)) retained
  pure (concatMap (payloadLimitRejections maximumSize) discarded)
