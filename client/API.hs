-- | Stable public surface for the core NATS client.
--
-- Constructors are intentionally hidden. This lets clients, messages, and
-- subscriptions acquire new capabilities without changing their public
-- representation.
module API
  ( Client
  , Message
  , MsgView
  , Subscription
  , subscriptionSid
  , NatsError (..)
  , Subject
  , Payload
  , Headers
  , PublishOption
  , SubscribeOption
  , RequestOption
  , UnsubscribeOption
  , PingOption
  , FlushOption
  , ResetOption
  , CloseOption
  , publish
  , subscribe
  , subscribeOnce
  , request
  , unsubscribe
  , newInbox
  , ping
  , flush
  , reset
  , close
  , subject
  , sid
  , replyTo
  , payload
  , headers
  , withSubscriptionExpiry
  , withQueueGroup
  , withReplyTo
  , withHeaders
  , withRequestTimeout
  , withRequestHeaders
  ) where

import           Client.API
    ( Client
    , CloseOption
    , FlushOption
    , Headers
    , Message
    , MsgView
    , NatsError (..)
    , Payload
    , PingOption
    , PublishOption
    , RequestOption
    , ResetOption
    , Subject
    , SubscribeOption
    , Subscription
    , UnsubscribeOption
    , close
    , flush
    , headers
    , newInbox
    , payload
    , ping
    , publish
    , replyTo
    , request
    , reset
    , sid
    , subject
    , subscribe
    , subscribeOnce
    , subscriptionSid
    , unsubscribe
    , withHeaders
    , withQueueGroup
    , withReplyTo
    , withRequestHeaders
    , withRequestTimeout
    , withSubscriptionExpiry
    )
