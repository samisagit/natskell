{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Queue.TransactionalQueue
  ( module Queue.TransactionalQueue.Types
  , newQ
  , queueApi
  ) where

import           Control.Concurrent.STM
import qualified Control.Monad
import           Queue.API
import           Queue.TransactionalQueue.Types
import           Queue.TransactionalQueueAPI    (TransactionalQueueAPI (..))
import           Transformers.Types

instance Transformer QueueItem where
  transform (QueueItem m) = transform m

newQ :: IO (Q QueueItem)
newQ = Q <$> newTBQueueIO 1000 <*> newEmptyTMVarIO

instance Transformer t => Queue (Q t) t where
  enqueue (Q q p) t = do
    isOpen <- atomically $ isEmptyTMVar p
    if isOpen
      then atomically $ do
        writeTBQueue q t
        return $ Right ()
      else return $ Left "Queue is closed"
  dequeue (Q q p) = do
    atomically $
      (Right <$> readTBQueue q)
      `orElse`
      (do
        isOpen <- isEmptyTMVar p
        check (not isOpen)
        return $ Left "Queue is closed")
  close (Q _ p) = atomically $ do
    _ <- tryTakeTMVar p
    putTMVar p ()
  open (Q _ p) = Control.Monad.void (atomically (tryTakeTMVar p))

queueApi :: TransactionalQueueAPI
queueApi = TransactionalQueueAPI
  { tqNew = newQ
  }
