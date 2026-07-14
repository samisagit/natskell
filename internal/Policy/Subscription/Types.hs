module Subscription.Types
  ( SubscribeConfig (..)
  , SubscriptionMeta (..)
  , PendingLimits (..)
  , defaultPendingLimits
  ) where

import           Data.Time.Clock (NominalDiffTime)
import           Types.Msg       (Subject)

data SubscribeConfig = SubscribeConfig
                         { expiry              :: Maybe NominalDiffTime
                         , subscribeQueueGroup :: Maybe Subject
                         }

data SubscriptionMeta = SubscriptionMeta
                          { subject    :: Subject
                          , queueGroup :: Maybe Subject
                          , isReply    :: Bool
                          }
  deriving (Eq, Show)

data PendingLimits = PendingLimits
                       { pendingMessageLimit :: Int
                       , pendingByteLimit    :: Int
                       }
  deriving (Eq, Show)

defaultPendingLimits :: PendingLimits
defaultPendingLimits = PendingLimits
  { pendingMessageLimit = 65536
  , pendingByteLimit = 64 * 1024 * 1024
  }
