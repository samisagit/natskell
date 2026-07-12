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
  , ConsumerPauseResponse (..)
  , ConsumerResetOption
  , ConsumerResetResponse (..)
  , ConsumerSequenceInfo (..)
  , DeleteConsumerResponse (..)
  , DeliverPolicy (..)
  , ReplayPolicy (..)
  , withConsumerAckPolicy
  , withConsumerAckWait
  , withConsumerDescription
  , withConsumerDeliverGroup
  , withConsumerDurableName
  , withConsumerDeliverPolicy
  , withConsumerDeliverSubject
  , withConsumerFilter
  , withConsumerHeadersOnly
  , withConsumerIdleHeartbeat
  , withConsumerInactiveThreshold
  , withConsumerListOffset
  , withConsumerMaxAckPending
  , withConsumerMaxDeliver
  , withConsumerMaxWaiting
  , withConsumerMemoryStorage
  , withConsumerName
  , withConsumerReplicas
  , withConsumerReplayPolicy
  , withConsumerResetSequence
  ) where

import           Data.Time.Clock          (UTCTime)
import           JetStream.Consumer.Types
import           JetStream.Error          (JetStreamError)
import           JetStream.Types          (ConsumerName, StreamName, Subject)

-- | Consumer management operations.
data ConsumerAPI = ConsumerAPI
                     { createConsumer :: StreamName -> [ConsumerConfigOption] -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Create an ephemeral or named consumer for a stream.
                     , createOrUpdateConsumer :: StreamName -> ConsumerName -> [ConsumerConfigOption] -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Create a named consumer or update it if it already exists.
                     , updateConsumer :: StreamName -> ConsumerName -> [ConsumerConfigOption] -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Update an existing named consumer.
                     , createDurableConsumer :: StreamName -> ConsumerName -> [ConsumerConfigOption] -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Create a durable consumer for a stream.
                     , createOrUpdateDurableConsumer :: StreamName -> ConsumerName -> [ConsumerConfigOption] -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Create a durable consumer or update it if it already exists.
                     , updateDurableConsumer :: StreamName -> ConsumerName -> [ConsumerConfigOption] -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Update an existing durable consumer.
                     , createPushConsumer :: StreamName -> ConsumerName -> Subject -> [ConsumerConfigOption] -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Create a named push consumer.
                     , createOrUpdatePushConsumer :: StreamName -> ConsumerName -> Subject -> [ConsumerConfigOption] -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Create a named push consumer or update it if it already exists.
                     , updatePushConsumer :: StreamName -> ConsumerName -> Subject -> [ConsumerConfigOption] -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Update an existing named push consumer.
                     , consumerInfo :: StreamName -> ConsumerName -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Fetch consumer configuration and state.
                     , pauseConsumer :: StreamName -> ConsumerName -> UTCTime -> IO (Either JetStreamError ConsumerPauseResponse)
                       -- ^ Pause a consumer until the given time.
                     , resumeConsumer :: StreamName -> ConsumerName -> IO (Either JetStreamError ConsumerPauseResponse)
                       -- ^ Resume a paused consumer.
                     , resetConsumer :: StreamName -> ConsumerName -> [ConsumerResetOption] -> IO (Either JetStreamError ConsumerResetResponse)
                       -- ^ Reset a consumer, optionally to a stream sequence.
                     , deleteConsumer :: StreamName -> ConsumerName -> IO (Either JetStreamError DeleteConsumerResponse)
                       -- ^ Delete a consumer from a stream.
                     , listConsumers :: StreamName -> [ConsumerListOption] -> IO (Either JetStreamError ConsumerListResponse)
                       -- ^ List consumers for a stream.
                     , consumerNames :: StreamName -> [ConsumerListOption] -> IO (Either JetStreamError ConsumerNamesResponse)
                     -- ^ List consumer names for a stream.
                     }
