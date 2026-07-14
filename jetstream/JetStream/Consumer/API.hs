module JetStream.Consumer.API
  ( ConsumerAPI
  , putConsumer
  , consumerInfo
  , pauseConsumer
  , resumeConsumer
  , resetConsumer
  , deleteConsumer
  , listConsumers
  , consumerNames
  , Consumer
  , consumerStreamName
  , consumerName
  , lookupConsumer
  , putConsumerHandle
  , getConsumerInfo
  , AckPolicy (..)
  , ConsumerAction (..)
  , ConsumerConfig
  , consumerConfigDurableName
  , consumerConfigName
  , consumerConfigDescription
  , consumerConfigDeliverSubject
  , consumerConfigDeliverGroup
  , consumerConfigDeliverPolicy
  , consumerConfigAckPolicy
  , consumerConfigReplayPolicy
  , consumerConfigFilterSubject
  , consumerConfigFilterSubjects
  , consumerConfigAckWait
  , consumerConfigMaxDeliver
  , consumerConfigMaxWaiting
  , consumerConfigMaxAckPending
  , consumerConfigInactiveThreshold
  , consumerConfigIdleHeartbeat
  , consumerConfigHeadersOnly
  , consumerConfigReplicas
  , consumerConfigMemoryStorage
  , ConsumerConfigOption
  , ConsumerFilter (..)
  , ConsumerKind (..)
  , ConsumerInfo
  , consumerInfoStreamName
  , consumerInfoName
  , consumerInfoCreated
  , consumerInfoConfig
  , consumerInfoDelivered
  , consumerInfoAckFloor
  , consumerInfoNumAckPending
  , consumerInfoNumRedelivered
  , consumerInfoNumWaiting
  , consumerInfoNumPending
  , ConsumerListOption
  , ConsumerListResponse
  , consumerListTotal
  , consumerListOffset
  , consumerListLimit
  , consumerListConsumers
  , ConsumerNamesResponse
  , consumerNamesTotal
  , consumerNamesOffset
  , consumerNamesLimit
  , consumerNamesConsumers
  , ConsumerPauseResponse
  , consumerPausePaused
  , consumerPauseUntilTime
  , consumerPauseRemaining
  , ConsumerResetOption
  , ConsumerResetResponse
  , consumerResetInfo
  , consumerResetResponseSequence
  , ConsumerSequenceInfo
  , consumerSequenceConsumer
  , consumerSequenceStream
  , consumerSequenceLast
  , ConsumerTarget (..)
  , DeleteConsumerResponse
  , deleteConsumerSuccess
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

import           JetStream.Consumer.Types
import           JetStream.Error          (JetStreamError)
import           JetStream.Types
    ( ConsumerName
    , JetStreamRequestOption
    , StreamName
    )

lookupConsumer
  :: ConsumerAPI
  -> StreamName
  -> ConsumerName
  -> [JetStreamRequestOption]
  -> IO (Either JetStreamError Consumer)
lookupConsumer api stream name requestOptions =
  fmap (fmap (const (Consumer stream name)))
    (consumerInfo api stream name requestOptions)

putConsumerHandle
  :: ConsumerAPI
  -> StreamName
  -> ConsumerAction
  -> ConsumerTarget
  -> ConsumerKind
  -> [ConsumerConfigOption]
  -> [JetStreamRequestOption]
  -> IO (Either JetStreamError Consumer)
putConsumerHandle api stream action target kind configOptions requestOptions =
  fmap (fmap toHandle)
    (putConsumer api stream action target kind configOptions requestOptions)
  where
    toHandle result = Consumer (consumerInfoStreamName result) (consumerInfoName result)

getConsumerInfo
  :: ConsumerAPI
  -> Consumer
  -> [JetStreamRequestOption]
  -> IO (Either JetStreamError ConsumerInfo)
getConsumerInfo api consumer =
  consumerInfo api (consumerStreamName consumer) (consumerName consumer)
