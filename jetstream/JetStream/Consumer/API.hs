module JetStream.Consumer.API
  ( ConsumerAPI (..)
  , AckPolicy (..)
  , ConsumerConfig (..)
  , ConsumerConfigOption
  , ConsumerFilter (..)
  , ConsumerInfo (..)
  , ConsumerListOption
  , ConsumerListResponse (..)
  , ConsumerNamesResponse (..)
  , ConsumerSequenceInfo (..)
  , DeleteConsumerResponse (..)
  , DeliverPolicy (..)
  , ReplayPolicy (..)
  , withConsumerAckPolicy
  , withConsumerAckWait
  , withConsumerDescription
  , withConsumerDurableName
  , withConsumerDeliverPolicy
  , withConsumerFilter
  , withConsumerInactiveThreshold
  , withConsumerListOffset
  , withConsumerMaxAckPending
  , withConsumerMaxDeliver
  , withConsumerName
  , withConsumerReplayPolicy
  ) where

import           JetStream.Consumer.Types
import           JetStream.Error          (JetStreamError)
import           JetStream.Types          (ConsumerName, StreamName)

-- | Consumer management operations.
data ConsumerAPI = ConsumerAPI
                     { createConsumer :: StreamName -> [ConsumerConfigOption] -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Create an ephemeral or named consumer for a stream.
                     , createDurableConsumer :: StreamName -> ConsumerName -> [ConsumerConfigOption] -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Create a durable consumer for a stream.
                     , consumerInfo :: StreamName -> ConsumerName -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Fetch consumer configuration and state.
                     , deleteConsumer :: StreamName -> ConsumerName -> IO (Either JetStreamError DeleteConsumerResponse)
                       -- ^ Delete a consumer from a stream.
                     , listConsumers :: StreamName -> [ConsumerListOption] -> IO (Either JetStreamError ConsumerListResponse)
                       -- ^ List consumers for a stream.
                     , consumerNames :: StreamName -> [ConsumerListOption] -> IO (Either JetStreamError ConsumerNamesResponse)
                     -- ^ List consumer names for a stream.
                     }
