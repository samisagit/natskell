-- | JetStream account and tier management contract.
module JetStream.API.Management
  ( ManagementAPI
  , accountInfo
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
  ) where

import           JetStream.Management.API (ManagementAPI, accountInfo)
import           JetStream.Types
    ( AccountAPIStats (..)
    , AccountInfo (..)
    , AccountLimits (..)
    , AccountTier (..)
    )
