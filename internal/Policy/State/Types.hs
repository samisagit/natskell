{-# LANGUAGE RankNTypes #-}

module State.Types
  ( TLSPublicKey
  , TLSPrivateKey
  , TLSCertData
  , TLSConfig (..)
  , defaultTLSConfig
  , ClientConfig (..)
  , ConnectError (..)
  , ConnectAttemptError (..)
  , ConnectFailure (..)
  , ServerError
  , ServerErrorKind (..)
  , serverErrorReason
  , serverErrorKind
  , serverErrorFromProtocol
  , ClientExitReason (..)
  , ConnectionEvent (..)
  , ConnectionState (..)
  ) where

import           Data.ByteString  (ByteString)
import qualified Data.ByteString  as BS
import           Lib.Logger.Types (LoggerConfig)
import           Types.Connect    (Connect)
import qualified Types.Err        as Err
import           Types.TLS

data ClientConfig = ClientConfig
                      { connectionAttempts     :: Int
                      , connectTimeoutMicros   :: Int
                      , callbackConcurrency    :: Int
                      , messageLimit           :: Int
                      , connectConfig          :: Connect
                      , loggerConfig           :: LoggerConfig
                      , tlsConfig              :: Maybe TLSConfig
                      , serverErrorHandler     :: ServerError -> IO ()
                      , connectionEventHandler :: ConnectionEvent -> IO ()
                      , exitAction             :: ClientExitReason -> IO ()
                      , connectOptions         :: [(String, Int)]
                      }

-- | Why an initial connection could not be established.
data ConnectError = ConnectNoServers
                  | ConnectAttemptsExhausted [ConnectAttemptError]
  deriving (Eq, Show)

-- | Failure from one server in the configured pool.
data ConnectAttemptError = ConnectAttemptError
                             { attemptedEndpoint :: (String, Int)
                             , attemptFailure    :: ConnectFailure
                             }
  deriving (Eq, Show)

-- | The stage at which a connection attempt failed.
data ConnectFailure = ConnectTransportFailure String
                    | ConnectTLSFailure String
                    | ConnectProtocolFailure String
                    | ConnectAuthenticationFailure String
                    | ConnectHandshakeTimeout
  deriving (Eq, Show)

-- | A server-reported protocol error.
--
-- Its wire representation is intentionally opaque so that new server error
-- categories do not force changes to the public type.
data ServerError = ServerError ServerErrorKind ByteString
  deriving (Eq)

instance Show ServerError where
  showsPrec precedence (ServerError _ reason) =
    showParen (precedence > applicationPrecedence) $
      showString "ServerError " . showsPrec (applicationPrecedence + 1) reason
    where
      applicationPrecedence = 10

-- | Stable categories for server-reported protocol errors.
data ServerErrorKind = ServerErrorAuthentication | ServerErrorPermission | ServerErrorProtocol | ServerErrorResourceLimit | ServerErrorConnection | ServerErrorUnknown
  deriving (Eq, Show)

serverErrorReason :: ServerError -> ByteString
serverErrorReason (ServerError _ reason) = reason

serverErrorKind :: ServerError -> ServerErrorKind
serverErrorKind (ServerError kind _) = kind

serverErrorFromProtocol :: Err.Err -> ServerError
serverErrorFromProtocol err =
  ServerError (classifyServerError err) (BS.copy (Err.errReason err))

classifyServerError :: Err.Err -> ServerErrorKind
classifyServerError err =
  case err of
    Err.ErrAuthViolation _      -> ServerErrorAuthentication
    Err.ErrAuthTimeout _        -> ServerErrorAuthentication
    Err.ErrAuthExpired _        -> ServerErrorAuthentication
    Err.ErrAuthRevoked _        -> ServerErrorAuthentication
    Err.ErrAccountAuthExpired _ -> ServerErrorAuthentication
    Err.ErrPermViolation _      -> ServerErrorPermission
    Err.ErrUnknownOp _          -> ServerErrorProtocol
    Err.ErrInvalidProtocol _    -> ServerErrorProtocol
    Err.ErrInvalidSubject _     -> ServerErrorProtocol
    Err.ErrMaxControlLineEx _   -> ServerErrorResourceLimit
    Err.ErrMaxConnsEx _         -> ServerErrorResourceLimit
    Err.ErrSlowConsumer _       -> ServerErrorResourceLimit
    Err.ErrMaxPayload _         -> ServerErrorResourceLimit
    Err.ErrRoutePortConn _      -> ServerErrorConnection
    Err.ErrTlsRequired _        -> ServerErrorConnection
    Err.ErrStaleConn _          -> ServerErrorConnection
    Err.ErrErr _                -> ServerErrorUnknown

data ClientExitReason = ExitClosedByUser
                      | ExitRetriesExhausted (Maybe String)
                      | ExitServerError ServerError
                      | ExitInboundMessageTooLarge Int Int
                      | ExitResetRequested
  deriving (Eq, Show)

-- | A completed connection lifecycle transition.
data ConnectionEvent = ConnectionEventDisconnected
                     | ConnectionEventReconnected
                     | ConnectionEventClosed ClientExitReason
  deriving (Eq, Show)

-- | Current lifecycle state of a client connection.
data ConnectionState = ConnectionConnecting
                     | ConnectionConnected
                     | ConnectionReconnecting
                     | ConnectionClosing ClientExitReason
                     | ConnectionClosed ClientExitReason
  deriving (Eq, Show)
