module Subscription.Types
  ( SubscribeConfig (..)
  , SubscriptionMeta (..)
  ) where

import           Data.Time.Clock (NominalDiffTime)
import           Types.Msg       (Subject)

newtype SubscribeConfig = SubscribeConfig { expiry :: Maybe NominalDiffTime }

data SubscriptionMeta = SubscriptionMeta
                          { subject    :: Subject
                          , queueGroup :: Maybe Subject
                          , isReply    :: Bool
                          }
  deriving (Eq, Show)
