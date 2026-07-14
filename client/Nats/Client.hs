-- | Core NATS client construction, configuration, and operations.
--
-- This module is the namespaced replacement for the historical @Client@ and
-- @API@ modules. Those modules remain available as compatibility imports.
module Nats.Client
  ( Client
  , Message
  , MsgView
  , Subscription
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
  , replyTo
  , payload
  , headers
  , withSubscriptionExpiry
  , withQueueGroup
  , withReplyTo
  , withHeaders
  , withRequestTimeout
  , withRequestHeaders
  , Server
  , ServerConfigError (..)
  , server
  , serverWithDefaultPort
  , serverHost
  , serverPort
  , connect
  , newClient
  , ConfigOption
  , withConnectName
  , withEcho
  , withAuthToken
  , withAuthTokenHandler
  , withUserPass
  , withUserPassHandler
  , withNKey
  , withNKeyHandler
  , withJWT
  , withJWTHandlers
  , withTLS
  , withTLSCert
  , withTLSRootCA
  , withTLSServerName
  , withTLSInsecure
  , withMinimumLogLevel
  , withLogAction
  , withConnectionAttempts
  , withConnectTimeoutMicros
  , withCallbackConcurrency
  , withMessageLimit
  , withBufferLimit
  , withExitAction
  , LogLevel (..)
  , LogEntry
  , leLevel
  , leMessage
  , leClientId
  , leConnectName
  , leServer
  , renderLogEntry
  , AuthTokenData
  , AuthTokenHandler
  , UserPassData
  , UserPassHandler
  , NKeyData
  , NKeyPublicKey
  , JWTTokenData
  , JWTHandler
  , SignatureHandler
  , AuthError (..)
  , TLSPublicKey
  , TLSPrivateKey
  , TLSCertData
  , ClientExitReason (..)
  , ServerError
  , serverErrorReason
  , ConnectError (..)
  , ConnectAttemptError
  , attemptedEndpoint
  , attemptFailure
  , ConnectFailure (..)
  ) where

import           API
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
    , subject
    , subscribe
    , subscribeOnce
    , unsubscribe
    , withHeaders
    , withQueueGroup
    , withReplyTo
    , withRequestHeaders
    , withRequestTimeout
    , withSubscriptionExpiry
    )
import           Client
    ( AuthError (..)
    , AuthTokenData
    , AuthTokenHandler
    , ClientExitReason (..)
    , ConfigOption
    , ConnectAttemptError
    , ConnectError (..)
    , ConnectFailure (..)
    , JWTHandler
    , JWTTokenData
    , LogEntry
    , LogLevel (..)
    , NKeyData
    , NKeyPublicKey
    , Server
    , ServerConfigError (..)
    , ServerError
    , SignatureHandler
    , TLSCertData
    , TLSPrivateKey
    , TLSPublicKey
    , UserPassData
    , UserPassHandler
    , attemptFailure
    , attemptedEndpoint
    , connect
    , leClientId
    , leConnectName
    , leLevel
    , leMessage
    , leServer
    , newClient
    , renderLogEntry
    , server
    , serverErrorReason
    , serverHost
    , serverPort
    , serverWithDefaultPort
    , withAuthToken
    , withAuthTokenHandler
    , withBufferLimit
    , withCallbackConcurrency
    , withConnectName
    , withConnectTimeoutMicros
    , withConnectionAttempts
    , withEcho
    , withExitAction
    , withJWT
    , withJWTHandlers
    , withLogAction
    , withMessageLimit
    , withMinimumLogLevel
    , withNKey
    , withNKeyHandler
    , withTLS
    , withTLSCert
    , withTLSInsecure
    , withTLSRootCA
    , withTLSServerName
    , withUserPass
    , withUserPassHandler
    )
