{-# LANGUAGE GADTs #-}

module Queue.API
  ( Queue (..)
  , QueueItem (..)
  , TryEnqueueResult (..)
  , isConnectionScoped
  , isWithinPayloadLimit
  , payloadLimitRejections
  ) where

import           Control.Concurrent.STM    (STM)
import           Transformers.Transformers (Transformer (..))

data QueueItem where QueueItem :: Transformer m => m -> QueueItem
                     QueueBatch :: [QueueItem] -> QueueItem
                     QueueConnectionScoped :: QueueItem -> QueueItem
                     QueuePayloadBound :: Int -> (Int -> IO ()) -> QueueItem -> QueueItem

instance Transformer QueueItem where
  transform (QueueItem item)             = transform item
  transform (QueueBatch items)           = mconcat (map transform items)
  transform (QueueConnectionScoped item) = transform item
  transform (QueuePayloadBound _ _ item) = transform item

isConnectionScoped :: QueueItem -> Bool
isConnectionScoped (QueueConnectionScoped _)    = True
isConnectionScoped (QueuePayloadBound _ _ item) = isConnectionScoped item
isConnectionScoped _                            = False

isWithinPayloadLimit :: Int -> QueueItem -> Bool
isWithinPayloadLimit maximumSize (QueuePayloadBound actualSize _ item) =
  actualSize <= maximumSize && isWithinPayloadLimit maximumSize item
isWithinPayloadLimit maximumSize (QueueConnectionScoped item) =
  isWithinPayloadLimit maximumSize item
isWithinPayloadLimit maximumSize (QueueBatch items) =
  all (isWithinPayloadLimit maximumSize) items
isWithinPayloadLimit _ (QueueItem _) = True

payloadLimitRejections :: Int -> QueueItem -> [IO ()]
payloadLimitRejections maximumSize (QueuePayloadBound actualSize reject item)
  | actualSize > maximumSize = [reject maximumSize]
  | otherwise = payloadLimitRejections maximumSize item
payloadLimitRejections maximumSize (QueueConnectionScoped item) =
  payloadLimitRejections maximumSize item
payloadLimitRejections maximumSize (QueueBatch items) =
  concatMap (payloadLimitRejections maximumSize) items
payloadLimitRejections _ (QueueItem _) = []

data TryEnqueueResult = TryEnqueued | TryQueueClosed | TryQueueFull
  deriving (Eq, Show)

data Queue = Queue
               { enqueue :: QueueItem -> IO (Either String ())
               , enqueueSTM :: QueueItem -> STM (Either String ())
               , tryEnqueue :: QueueItem -> IO TryEnqueueResult
               , tryEnqueueSTM :: QueueItem -> STM TryEnqueueResult
               , dequeue :: IO (Either String QueueItem)
               , close :: IO ()
               , open :: IO ()
               , openSTM :: STM ()
               , discardConnectionScoped :: IO ()
               , closeAndDiscardConnectionScoped :: STM ()
               , closeAndDiscardAll :: STM ()
               , discardOversizedPayloads :: Int -> STM [IO ()]
               }
