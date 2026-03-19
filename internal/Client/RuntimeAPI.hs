{-# LANGUAGE RankNTypes #-}

module Client.RuntimeAPI
  ( User
  , Pass
  , UserPassData
  , NKeyData
  , AuthTokenData
  , JWTTokenData
  , TLSPublicKey
  , TLSPrivateKey
  , TLSCertData
  , Auth (..)
  , Config (..)
  , ClientState (..)
  , ConfigState (..)
  , RuntimeAPI (..)
  ) where

import           Client.LifecycleAPI
    ( ClientExitReason
    , LifecycleState
    )
import           Client.SubscriptionAPI         (SubscriptionState)
import           Control.Concurrent.STM         (TQueue, TVar)
import qualified Data.ByteString                as BS
import           Lib.Logger.Types               (AppM, LogContext, LoggerConfig)
import           Network.ConnectionAPI          (Conn)
import           Nuid.Types                     (Nuid)
import           Queue.TransactionalQueue.Types (Q, QueueItem)
import           Sid.Types                      (SIDCounter)
import           Types.Connect.Types            (Connect)
import qualified Types.Info.Types               as I

type User = BS.ByteString
type Pass = BS.ByteString
type UserPassData = (User, Pass)

type NKeyData = BS.ByteString

type AuthTokenData = BS.ByteString

type JWTTokenData = BS.ByteString

type TLSPublicKey = BS.ByteString
type TLSPrivateKey = BS.ByteString
type TLSCertData = (TLSPublicKey, TLSPrivateKey)

data Auth = None
          | UserPass UserPassData
          | NKey NKeyData
          | AuthToken AuthTokenData
          | JWT JWTTokenData
  deriving (Eq, Show)

data Config = Config
                { connectionAttempts  :: Int
                , callbackConcurrency :: Int
                , bufferLimit         :: Int
                , connectConfig       :: Connect
                , loggerConfig        :: LoggerConfig
                , auth                :: Auth
                , tlsCert             :: Maybe TLSCertData
                , exitAction          :: ClientExitReason -> IO ()
                , connectOptions      :: [(String, Int)]
                }

data ConfigState = ConfigState
                     { cfgConfig     :: Config
                     , cfgServerInfo :: Maybe I.Info
                     }

-- | Internal client state used to interact with the NATS server.
data ClientState = ClientState
                     { queue               :: Q QueueItem
                     , subscriptions       :: TVar SubscriptionState
                     , pings               :: TQueue (IO ())
                     , callbackQueue       :: TQueue (IO ())
                     , callbackPending     :: TVar Int
                     , connectedOnce       :: TVar Bool
                     , sidCounter          :: TVar SIDCounter
                     , inboxNuid           :: TVar Nuid
                     , configState         :: TVar ConfigState
                     , connectionAttempts' :: TVar Int
                     , lifecycle           :: TVar LifecycleState
                     , conn                :: Conn
                     , logContext          :: TVar LogContext
                     }

data RuntimeAPI = RuntimeAPI
                    { runtimeReadConfigState :: ClientState -> IO ConfigState
                    , runtimeReadConfig :: ClientState -> IO Config
                    , runtimeRunClient :: forall a. ClientState -> AppM a -> IO a
                    , runtimeWriteToClientQueue :: ClientState -> QueueItem -> IO ()
                    }
