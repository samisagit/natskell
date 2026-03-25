{-# LANGUAGE GADTs #-}

module Queue.API
  ( Queue (..)
  , QueueItem (..)
  ) where

import           Transformers.Transformers (Transformer (..))

data QueueItem where QueueItem :: Transformer m => m -> QueueItem

instance Transformer QueueItem where
  transform (QueueItem item) = transform item

data Queue = Queue
               { queueEnqueue :: QueueItem -> IO (Either String ())
               , queueDequeue :: IO (Either String QueueItem)
               , queueClose   :: IO ()
               , queueOpen    :: IO ()
               }
