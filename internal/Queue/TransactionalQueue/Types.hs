{-# LANGUAGE GADTs #-}

module Queue.TransactionalQueue.Types
  ( QueueItem (..)
  , Q (..)
  ) where

import           Control.Concurrent.STM    (TBQueue, TMVar)
import           Transformers.Transformers (Transformer (..))

data QueueItem where QueueItem :: Transformer m => m -> QueueItem

data Q t = Q (TBQueue t) (TMVar ())

instance Transformer QueueItem where
  transform (QueueItem item) = transform item
