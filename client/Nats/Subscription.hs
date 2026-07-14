-- | Core NATS subscription identifiers and options.
module Nats.Subscription
  ( Subscription
  , SubscribeOption
  , UnsubscribeOption
  , subscribe
  , subscribeOnce
  , unsubscribe
  , withQueueGroup
  , withSubscriptionExpiry
  ) where

import           API
    ( SubscribeOption
    , Subscription
    , UnsubscribeOption
    , subscribe
    , subscribeOnce
    , unsubscribe
    , withQueueGroup
    , withSubscriptionExpiry
    )
