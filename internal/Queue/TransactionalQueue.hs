{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Queue.TransactionalQueue where

import           Control.Concurrent.STM
import           Queue.API
import           Transformers.Transformers

data QueueItem = forall m. Transformer m => QueueItem m

instance Transformer QueueItem where
  transform (QueueItem m) = transform m

data Q t = Q (TBQueue t) (TVar Bool)

newQ :: IO (Q QueueItem)
newQ = Q <$> newTBQueueIO 1000 <*> newTVarIO False

instance Transformer t => Queue (Q t) t where
  enqueue (Q q p) t = do
    poisoned <- readTVarIO p
    if poisoned
      then return $ Left "Queue is poisoned"
      else atomically $ do
        writeTBQueue q t
        return $ Right ()
  dequeue (Q q p) = do
    atomically $
      (Right <$> readTBQueue q)
      `orElse`
      (do
        poisoned <- readTVar p
        check poisoned
        return $ Left "Queue is poisoned")
  isEmpty (Q q p) = do
    poisoned <- readTVarIO p
    if poisoned
      then return True
      else atomically $ isEmptyTBQueue q
  close (Q _ p) = atomically $ writeTVar p True

