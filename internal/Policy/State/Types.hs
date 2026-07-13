{-# LANGUAGE RankNTypes #-}

module State.Types
  ( TLSPublicKey
  , TLSPrivateKey
  , TLSCertData
  , ClientConfig (..)
  , ConnectError (..)
  , ConnectAttemptError (..)
  , ConnectFailure (..)
  , ClientExitReason (..)
  , ClientStatus (..)
  ) where

import qualified Data.ByteString  as BS
import           Lib.Logger.Types (LoggerConfig)
import           Types.Connect    (Connect)
import qualified Types.Err        as Err

type TLSPublicKey = BS.ByteString
type TLSPrivateKey = BS.ByteString
type TLSCertData = (TLSPublicKey, TLSPrivateKey)

data ClientConfig = ClientConfig
                      { connectionAttempts   :: Int
                      , connectTimeoutMicros :: Int
                      , callbackConcurrency  :: Int
                      , bufferLimit          :: Int
                      , connectConfig        :: Connect
                      , loggerConfig         :: LoggerConfig
                      , tlsCert              :: Maybe TLSCertData
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

data ClientExitReason = ExitClosedByUser
                      | ExitRetriesExhausted (Maybe String)
                      | ExitServerError Err.Err
                      | ExitResetRequested
  deriving (Eq, Show)

data ClientStatus = Running
                  | Closing ClientExitReason
                  | Closed ClientExitReason
  deriving (Eq, Show)
