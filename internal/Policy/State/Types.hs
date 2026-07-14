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
  , serverErrorReason
  , serverErrorFromProtocol
  , ClientExitReason (..)
  , ClientStatus (..)
  ) where

import           Data.ByteString  (ByteString)
import           Lib.Logger.Types (LoggerConfig)
import           Types.Connect    (Connect)
import qualified Types.Err        as Err
import           Types.TLS

data ClientConfig = ClientConfig
                      { connectionAttempts   :: Int
                      , connectTimeoutMicros :: Int
                      , callbackConcurrency  :: Int
                      , bufferLimit          :: Int
                      , connectConfig        :: Connect
                      , loggerConfig         :: LoggerConfig
                      , tlsConfig            :: Maybe TLSConfig
                      , exitAction           :: ClientExitReason -> IO ()
                      , connectOptions       :: [(String, Int)]
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
newtype ServerError = ServerError ByteString
  deriving (Eq, Show)

serverErrorReason :: ServerError -> ByteString
serverErrorReason (ServerError reason) = reason

serverErrorFromProtocol :: Err.Err -> ServerError
serverErrorFromProtocol = ServerError . Err.errReason

data ClientExitReason = ExitClosedByUser
                      | ExitRetriesExhausted (Maybe String)
                      | ExitServerError ServerError
                      | ExitResetRequested
  deriving (Eq, Show)

data ClientStatus = Running
                  | Closing ClientExitReason
                  | Closed ClientExitReason
  deriving (Eq, Show)
