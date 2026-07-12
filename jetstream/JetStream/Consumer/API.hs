module JetStream.Consumer.API
  ( ConsumerAPI (..)
  , AckPolicy (..)
  , ConsumerAction (..)
  , ConsumerConfig (..)
  , ConsumerConfigOption
  , ConsumerFilter (..)
  , ConsumerKind (..)
  , ConsumerInfo (..)
  , ConsumerListOption
  , ConsumerListResponse (..)
  , ConsumerNamesResponse (..)
  , ConsumerPauseResponse (..)
  , ConsumerResetOption
  , ConsumerResetResponse (..)
  , ConsumerSequenceInfo (..)
  , ConsumerTarget (..)
  , DeleteConsumerResponse (..)
  , DeliverPolicy (..)
  , ReplayPolicy (..)
  , withConsumerAckPolicy
  , withConsumerAckWait
  , withConsumerDescription
  , withConsumerDeliverGroup
  , withConsumerDeliverPolicy
  , withConsumerFilter
  , withConsumerHeadersOnly
  , withConsumerIdleHeartbeat
  , withConsumerInactiveThreshold
  , withConsumerListOffset
  , withConsumerMaxAckPending
  , withConsumerMaxDeliver
  , withConsumerMaxWaiting
  , withConsumerMemoryStorage
  , withConsumerReplicas
  , withConsumerReplayPolicy
  , withConsumerResetSequence
  ) where

import           Data.Time.Clock          (UTCTime)
import           JetStream.Consumer.Types
import           JetStream.Error          (JetStreamError)
import           JetStream.Types          (ConsumerName, StreamName)

-- | Consumer management operations.
data ConsumerAPI = ConsumerAPI
                     { putConsumer :: StreamName -> ConsumerAction -> ConsumerTarget -> ConsumerKind -> [ConsumerConfigOption] -> IO (Either JetStreamError ConsumerInfo)
                       -- ^ Create, update, or create-or-update a stream consumer.
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
