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
  , ConnectionState (..)
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
  , connectionState
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
  , withPingTimeout
  , withFlushTimeout
  ) where

import           Client.API
    ( Client
    , CloseOption
    , ConnectionState (..)
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
    , connectionState
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
    , withFlushTimeout
    , withHeaders
    , withPingTimeout
    , withQueueGroup
    , withReplyTo
    , withRequestHeaders
    , withRequestTimeout
    , withSubscriptionExpiry
    )
