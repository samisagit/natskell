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
               { enqueue :: QueueItem -> IO (Either String ())
               , dequeue :: IO (Either String QueueItem)
               , close   :: IO ()
               , open    :: IO ()
               }
