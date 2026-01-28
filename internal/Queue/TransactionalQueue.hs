{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Queue.TransactionalQueue where

import           Control.Concurrent.STM
import qualified Control.Monad
import           Queue.API
import           Transformers.Transformers

data QueueItem where QueueItem :: Transformer m => m -> QueueItem

instance Transformer QueueItem where
  transform (QueueItem m) = transform m

data Q t = Q (TBQueue t) (TMVar ())

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
