module JetStream.Consumer.API
  ( ConsumerAPI (..)
  , module JetStream.Consumer.Types
  ) where

import           JetStream.Consumer.Types
import           JetStream.Error          (JetStreamError)
import           JetStream.Types          (ConsumerName, StreamName)

-- | Consumer management operations.
data ConsumerAPI = ConsumerAPI
                     { createConsumer :: StreamName -> ConsumerConfig -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Create an ephemeral or named consumer for a stream.
                     , createDurableConsumer :: StreamName -> ConsumerName -> ConsumerConfig -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Create a durable consumer for a stream.
                     , consumerInfo :: StreamName -> ConsumerName -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Fetch consumer configuration and state.
                     , deleteConsumer :: StreamName -> ConsumerName -> IO (Either JetStreamError DeleteConsumerResponse)
                       -- ^ Delete a consumer from a stream.
                     , listConsumers :: StreamName -> ConsumerListRequest -> IO (Either JetStreamError ConsumerListResponse)
                       -- ^ List consumers for a stream.
                     , consumerNames :: StreamName -> ConsumerNamesRequest -> IO (Either JetStreamError ConsumerNamesResponse)
                     -- ^ List consumer names for a stream.
                     }
