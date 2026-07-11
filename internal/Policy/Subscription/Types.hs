module Subscription.Types
  ( SubscribeConfig (..)
  , SubscriptionMeta (..)
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
