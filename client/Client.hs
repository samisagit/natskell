-- | High-level client implementation for NATS.
module Client
  ( newClient
  , ConfigOption
  , withConnectName
  , withEcho
  , withAuthToken
  , withUserPass
  , withNKey
  , withJWT
  , withTLSCert
  , withMinimumLogLevel
  , withLogAction
  , withConnectionAttempts
  , withCallbackConcurrency
  , withBufferLimit
  , withExitAction
  , LogLevel (..)
  , LogEntry (..)
  , renderLogEntry
  , AuthTokenData
  , UserPassData
  , NKeyData
  , JWTTokenData
  , TLSPublicKey
  , TLSPrivateKey
  , TLSCertData
  , ClientExitReason (..)
  ) where

import           API                         (Client (..), MsgView (..))
import           Client.Auth
    ( defaultConnect
    , logAuthMethod
    , logTlsConfig
    )
import           Client.CallbacksAPI
    ( CallbacksAPI (callbacksStartWorkers)
    )
import           Client.Internal
    ( callbacksApi
    , closeClient
    , connectionApi
    , flushInternal
    , lifecycleApi
    , nuidApi
    , pingInternal
    , publishInternal
    , queueApi
    , resetClient
    , retryLoop
    , runtimeApi
    , sidApi
    )
import           Client.LifecycleAPI
    ( ClientExitReason (..)
    , LifecycleAPI (lifecycleWaitForClosed, lifecycleWaitForServerInfo)
    , LifecycleState (Running)
    )
import           Client.PublishAPI           (defaultPublishConfig)
import           Client.RuntimeAPI
    ( Auth (..)
    , AuthTokenData
    , ClientState (..)
    , Config (..)
    , ConfigState (..)
    , JWTTokenData
    , NKeyData
    , RuntimeAPI (runtimeRunClient)
    , TLSCertData
    , TLSPrivateKey
    , TLSPublicKey
    , UserPassData
    )
import           Client.Subscription
    ( subscribeInternal
    , unsubscribeInternal
    )
import           Client.SubscriptionAPI
    ( SubscribeConfig (..)
    , SubscriptionState (..)
    , emptySubscriptionGC
    )
import           Control.Concurrent          (forkIO)
import           Control.Concurrent.STM
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BC
import           Lib.CallOption              (CallOption, applyCallOptions)
import           Lib.Logger
    ( LogContext (..)
    , LogEntry (..)
    , LogLevel (..)
    , LoggerConfig (..)
    , defaultLogger
    , newLogContext
    , renderLogEntry
    , updateLogContext
    )
import           Network.ConnectionAPI       (ConnectionAPI (connectionNew))
import           NuidAPI                     (NuidAPI (nuidNew))
import           Queue.TransactionalQueueAPI (TransactionalQueueAPI (tqNew))
import           SidAPI                      (SidAPI (sidInitial))
import qualified Types.Connect               as Connect
import qualified Types.Msg                   as Msg

-- | newClient creates a new client with optional overrides to default settings.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- servers = [("127.0.0.1", 4222)]
-- configOptions = [withConnectName "example-client"]
-- client <- newClient servers configOptions
-- @
newClient :: [(String, Int)] -> [ConfigOption] -> IO Client
newClient servers configOptions = do
  dl <- defaultLogger
  ctx <- newLogContext
  let defaultConfig = applyCallOptions configOptions Config
        { connectConfig = defaultConnect
        , auth = None
        , tlsCert = Nothing
        , loggerConfig = dl
        , connectionAttempts = 5
        , callbackConcurrency = 1
        , bufferLimit = 4096
        , exitAction = const (return ())
        , connectOptions = servers
        }
      subscriptionState = SubscriptionState mempty emptySubscriptionGC mempty
      configState = ConfigState defaultConfig Nothing
  updateLogContext ctx (\tmp -> tmp
    { lcConnectName = fmap BC.unpack (Connect.name (connectConfig defaultConfig))
    })

  c <- connectionNew connectionApi

  clientState <- ClientState
    <$> tqNew queueApi
    <*> newTVarIO subscriptionState
    <*> newTQueueIO
    <*> newTQueueIO
    <*> newTVarIO 0
    <*> newTVarIO False
    <*> newTVarIO (sidInitial sidApi)
    <*> (newTVarIO =<< nuidNew nuidApi)
    <*> newTVarIO configState
    <*> newTVarIO (connectionAttempts defaultConfig)
    <*> newTVarIO Running
    <*> pure c
    <*> pure ctx

  runtimeRunClient runtimeApi clientState $ do
    logAuthMethod (auth defaultConfig)
    logTlsConfig (tlsCert defaultConfig)
  callbacksStartWorkers callbacksApi clientState
  forkIO $ retryLoop clientState

  -- ensure that the server info is initialized
  atomically $
    lifecycleWaitForServerInfo lifecycleApi clientState
      `orElse` lifecycleWaitForClosed lifecycleApi clientState

  let clientApi =
        Client
          { publish = \subject publishOptions -> do
              let cfg = applyCallOptions publishOptions defaultPublishConfig
              publishInternal clientState subject cfg
          , subscribe = \subject subscribeOptions callback -> do
              let cfg = applyCallOptions subscribeOptions defaultSubscribeConfig
                  internalCallback = callback . fmap (\msg -> MsgView
                    { subject = Msg.subject msg
                    , sid = Msg.sid msg
                    , replyTo = Msg.replyTo msg
                    , payload = Msg.payload msg
                    , headers = Msg.headers msg
                    })
              subscribeInternal runtimeApi sidApi False clientState subject cfg internalCallback
          , request = \subject subscribeOptions callback -> do
              let cfg = applyCallOptions subscribeOptions defaultSubscribeConfig
                  internalCallback = callback . fmap (\msg -> MsgView
                    { subject = Msg.subject msg
                    , sid = Msg.sid msg
                    , replyTo = Msg.replyTo msg
                    , payload = Msg.payload msg
                    , headers = Msg.headers msg
                    })
              subscribeInternal runtimeApi sidApi True clientState subject cfg internalCallback
          , unsubscribe = unsubscribeInternal runtimeApi clientState
          , ping = pingInternal clientState
          , flush = flushInternal clientState
          , reset = resetClient clientState
          , close = closeClient clientState
          }
  pure clientApi

type ConfigOption = CallOption Config

-- | withConnectName sets the client name sent in CONNECT.
-- Default: no CONNECT name is sent.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- client <- newClient [(\"127.0.0.1\", 4222)] [withConnectName \"example-client\"]
-- @
withConnectName :: BS.ByteString -> ConfigOption
withConnectName name config = config { connectConfig = (connectConfig config) { Connect.name = Just name } }

-- | withEcho configures whether the server echoes messages from this connection.
-- Default: True (messages sent by this client are echoed back).
withEcho :: Bool -> ConfigOption
withEcho enabled config = config { connectConfig = (connectConfig config) { Connect.echo = Just enabled } }

-- | withAuthToken configures token authentication.
-- Default: no authentication is configured.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- client <- newClient [(\"127.0.0.1\", 4222)] [withAuthToken \"s3cr3t\"]
-- @
withAuthToken :: AuthTokenData -> ConfigOption
withAuthToken token config = config { auth = AuthToken token }

-- | withUserPass configures user/password authentication.
-- Default: no authentication is configured.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- client <- newClient [(\"127.0.0.1\", 4222)] [withUserPass (\"alice\", \"secret\")]
-- @
withUserPass :: UserPassData -> ConfigOption
withUserPass (user, pass) config = config { auth = UserPass (user, pass) }

-- | withNKey configures NKey authentication.
-- Default: no authentication is configured.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- client <- newClient [(\"127.0.0.1\", 4222)] [withNKey \"SU...\"]
-- @
withNKey :: NKeyData -> ConfigOption
withNKey nkey config = config { auth = NKey nkey }

-- | withJWT configures JWT authentication.
-- Default: no authentication is configured.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import qualified Data.ByteString as BS
--
-- creds <- BS.readFile \"user.creds\"
-- client <- newClient [(\"127.0.0.1\", 4222)] [withJWT creds]
-- @
withJWT :: JWTTokenData -> ConfigOption
withJWT jwt config = config { auth = JWT jwt }

-- | withTLSCert configures client TLS credentials.
-- Default: no TLS credentials are configured.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import qualified Data.ByteString as BS
--
-- certPem <- BS.readFile \"client.pem\"
-- keyPem <- BS.readFile \"client.key\"
-- client <- newClient [(\"127.0.0.1\", 4222)] [withTLSCert (certPem, keyPem)]
-- @
withTLSCert :: TLSCertData -> ConfigOption
withTLSCert (pubKey, privKey) config = config { tlsCert = Just (pubKey, privKey) }

-- | withMinimumLogLevel sets the minimum level that will be emitted by the default logger.
-- Default: 'Info'.
--
-- __Examples:__
--
-- @
-- client <- newClient [(\"127.0.0.1\", 4222)] [withMinimumLogLevel Info]
-- @
withMinimumLogLevel :: LogLevel -> ConfigOption
withMinimumLogLevel minimumLogLevel config =
  config { loggerConfig = (loggerConfig config) { minLogLevel = minimumLogLevel } }

-- | withLogAction replaces the default log sink.
-- Default: logs are rendered with 'renderLogEntry' and written with 'putStrLn'.
--
-- __Examples:__
--
-- @
-- client <- newClient [(\"127.0.0.1\", 4222)] [withLogAction print]
-- @
withLogAction :: (LogEntry -> IO ()) -> ConfigOption
withLogAction logAction config =
  config { loggerConfig = (loggerConfig config) { logFn = logAction } }

-- | withConnectionAttempts sets the number of connection retries.
-- Default: 5.
--
-- __Examples:__
--
-- @
-- client <- newClient [(\"127.0.0.1\", 4222)] [withConnectionAttempts 10]
-- @
withConnectionAttempts :: Int -> ConfigOption
withConnectionAttempts attempts config = config { connectionAttempts = attempts }

-- | withCallbackConcurrency sets the number of worker threads used for callbacks.
-- Default: 1 (callbacks run serially). Values less than 1 are treated as 1.
--
-- __Examples:__
--
-- @
-- client <- newClient [(\"127.0.0.1\", 4222)] [withCallbackConcurrency 4]
-- @
withCallbackConcurrency :: Int -> ConfigOption
withCallbackConcurrency concurrency config = config { callbackConcurrency = concurrency }

-- | withBufferLimit sets the maximum outbound message size and parser buffer size in bytes.
-- Default: 4096. Values less than 1 are treated as 1.
--
-- __Examples:__
--
-- @
-- client <- newClient [(\"127.0.0.1\", 4222)] [withBufferLimit 8192]
-- @
withBufferLimit :: Int -> ConfigOption
withBufferLimit limit config = config { bufferLimit = max 1 limit }

-- | withExitAction runs a callback when the client exits.
-- Default: no-op.
--
-- __Examples:__
--
-- @
-- client <- newClient [(\"127.0.0.1\", 4222)] [withExitAction print]
-- @
withExitAction :: (ClientExitReason -> IO ()) -> ConfigOption
withExitAction action config = config { exitAction = action }

defaultSubscribeConfig :: SubscribeConfig
defaultSubscribeConfig = SubscribeConfig Nothing
