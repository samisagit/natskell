module Queue.TransactionalQueueAPI
  ( Q
  , QueueItem (..)
  , TransactionalQueueAPI (..)
  ) where

import           Queue.TransactionalQueue.Types (Q, QueueItem (..))

-- | API wrapper for transactional queue capabilities.
newtype TransactionalQueueAPI = TransactionalQueueAPI { tqNew :: IO (Q QueueItem) }
