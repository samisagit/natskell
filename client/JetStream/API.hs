-- | Capability record for the JetStream client surface.
module JetStream.API
  ( JetStream
  , streams
  , consumers
  , publisher
  , messages
  , accountInfo
  , JetStreamRequestOption
  , withRequestTimeout
  , Sequence
  , sequenceFromWord64
  , sequenceToWord64
  , AccountAPIStats
  , accountAPILevel
  , accountAPITotal
  , accountAPIErrors
  , accountAPIInflight
  , AccountInfo
  , accountInfoTier
  , accountInfoDomain
  , accountInfoAPI
  , accountInfoTiers
  , AccountLimits
  , accountLimitsMaxMemory
  , accountLimitsMaxStorage
  , accountLimitsMaxStreams
  , accountLimitsMaxConsumers
  , accountLimitsMaxAckPending
  , accountLimitsMemoryMaxStreamBytes
  , accountLimitsStorageMaxStreamBytes
  , accountLimitsMaxBytesRequired
  , AccountTier
  , accountTierMemory
  , accountTierStorage
  , accountTierReservedMemory
  , accountTierReservedStorage
  , accountTierStreams
  , accountTierConsumers
  , accountTierLimits
  , JetStreamApiError
  , apiErrorCode
  , apiErrorCodeDetail
  , apiErrorDescription
  , JetStreamError (..)
  , StreamName
  , ConsumerName
  , Subject
  , Payload
  , module JetStream.Consumer.API
  , module JetStream.Message.API
  , module JetStream.Publish.API
  , module JetStream.Stream.API
  ) where

import           JetStream.Consumer.API
import           JetStream.Error
    ( JetStreamApiError
    , JetStreamError (..)
    , apiErrorCode
    , apiErrorCodeDetail
    , apiErrorDescription
    )
import           JetStream.Message.API
import           JetStream.Options
    ( JetStream
    , accountInfo
    , consumers
    , messages
    , publisher
    , streams
    )
import           JetStream.Publish.API
import           JetStream.Stream.API
import           JetStream.Types
    ( AccountAPIStats (..)
    , AccountInfo (..)
    , AccountLimits (..)
    , AccountTier (..)
    , ConsumerName
    , JetStreamRequestOption
    , Payload
    , Sequence
    , StreamName
    , Subject
    , sequenceFromWord64
    , sequenceToWord64
    , withRequestTimeout
    )
