{-# LANGUAGE RankNTypes #-}

module State.Types
  ( TLSPublicKey
  , TLSPrivateKey
  , TLSCertData
  , ClientConfig (..)
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
                      { connectionAttempts  :: Int
                      , callbackConcurrency :: Int
                      , bufferLimit         :: Int
                      , connectConfig       :: Connect
                      , loggerConfig        :: LoggerConfig
                      , tlsCert             :: Maybe TLSCertData
                      , exitAction          :: ClientExitReason -> IO ()
                      , connectOptions      :: [(String, Int)]
                      }

data ClientExitReason = ExitClosedByUser
                      | ExitRetriesExhausted (Maybe String)
                      | ExitServerError Err.Err
                      | ExitResetRequested
  deriving (Eq, Show)

data ClientStatus = Running
                  | Closing ClientExitReason
                  | Closed ClientExitReason
  deriving (Eq, Show)
