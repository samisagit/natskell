-- | Construct a NATS client satisfying the contract in "API".
module Client
  ( Server
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
  , withPendingDeliveryLimits
  , withErrorHandler
  , withServerErrorHandler
  , withConnectionEventHandler
  , withBufferLimit
  , withExitAction
  , LogLevel (..)
  , LogEntry (..)
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
  , TLSConfig (..)
  , ClientExitReason (..)
  , ConnectionEvent (..)
  , ServerError
  , ServerErrorKind (..)
  , serverErrorReason
  , serverErrorKind
  , ConnectError (..)
  , ConnectAttemptError (..)
  , ConnectFailure (..)
  ) where

import           Client.Implementation
