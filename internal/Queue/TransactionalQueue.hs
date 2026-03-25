module Queue.TransactionalQueue
  ( newQueue
  ) where

import           Control.Concurrent.STM
import qualified Control.Monad
import           Queue.API

data TransactionalQueue = TransactionalQueue
                            { transactionalQueueItems  :: TBQueue QueueItem
                            , transactionalQueueClosed :: TMVar ()
                            }

newQueue :: IO Queue
newQueue = do
  queueItems <- newTBQueueIO 1000
  queueClosed <- newEmptyTMVarIO
  let transactionalQueue =
        TransactionalQueue
          { transactionalQueueItems = queueItems
          , transactionalQueueClosed = queueClosed
          }
  pure Queue
    { queueEnqueue = enqueue transactionalQueue
    , queueDequeue = dequeue transactionalQueue
    , queueClose = close transactionalQueue
    , queueOpen = open transactionalQueue
    }

enqueue :: TransactionalQueue -> QueueItem -> IO (Either String ())
enqueue transactionalQueue item = do
  isOpen <- atomically $ isEmptyTMVar (transactionalQueueClosed transactionalQueue)
  if isOpen
    then atomically $ do
      writeTBQueue (transactionalQueueItems transactionalQueue) item
      pure (Right ())
    else pure (Left "Queue is closed")

dequeue :: TransactionalQueue -> IO (Either String QueueItem)
dequeue transactionalQueue =
  atomically $
    (Right <$> readTBQueue (transactionalQueueItems transactionalQueue))
    `orElse`
    (do
      isOpen <- isEmptyTMVar (transactionalQueueClosed transactionalQueue)
      check (not isOpen)
      pure (Left "Queue is closed"))

close :: TransactionalQueue -> IO ()
close transactionalQueue = atomically $ do
  _ <- tryTakeTMVar (transactionalQueueClosed transactionalQueue)
  putTMVar (transactionalQueueClosed transactionalQueue) ()

open :: TransactionalQueue -> IO ()
open transactionalQueue =
  Control.Monad.void (atomically (tryTakeTMVar (transactionalQueueClosed transactionalQueue)))
