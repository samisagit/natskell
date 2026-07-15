module Subscription.Types
  ( SubscribeConfig (..)
  , SubscriptionKind (..)
  , SubscriptionMeta (..)
  , isOneShotSubscription
  , isResumableSubscription
  , tracksSubscriptionExpiry
  , PendingLimits (..)
  , defaultPendingLimits
  ) where

import           Data.Time.Clock (NominalDiffTime)
import           Types.Msg       (Subject)

data SubscribeConfig = SubscribeConfig
                         { expiry              :: Maybe NominalDiffTime
                         , subscribeQueueGroup :: Maybe Subject
                         }

data SubscriptionKind = StandardSubscription | OneShotSubscription | RequestReplySubscription
  deriving (Eq, Show)

data SubscriptionMeta = SubscriptionMeta
                          { subject    :: Subject
                          , queueGroup :: Maybe Subject
                          , kind       :: SubscriptionKind
                          }
  deriving (Eq, Show)

isOneShotSubscription :: SubscriptionMeta -> Bool
isOneShotSubscription meta =
  case kind meta of
    StandardSubscription     -> False
    OneShotSubscription      -> True
    RequestReplySubscription -> True

isResumableSubscription :: SubscriptionMeta -> Bool
isResumableSubscription meta = kind meta /= RequestReplySubscription

tracksSubscriptionExpiry :: SubscriptionMeta -> Bool
tracksSubscriptionExpiry meta = kind meta == OneShotSubscription

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
