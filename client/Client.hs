{-# LANGUAGE OverloadedStrings #-}

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
  , withConnectTimeoutMicros
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
  , ConnectError (..)
  , ConnectAttemptError (..)
  , ConnectFailure (..)
  ) where

import qualified Auth.Jwt                 as AuthJwt
import qualified Auth.NKey                as AuthNKey
import qualified Auth.None                as AuthNone
import qualified Auth.Token               as AuthToken
import           Auth.Types
    ( Auth
    , AuthTokenData
    , JWTTokenData
    , NKeyData
    , UserPassData
    )
import qualified Auth.UserPass            as AuthUserPass
import           Client.API               (Client (..), MsgView (..))
import           Control.Concurrent       (forkIO)
import           Control.Concurrent.STM
import           Control.Exception        (SomeException, displayException)
import           Control.Monad            (void, when)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BC
import           Engine                   (closeClient, resetClient, runEngine)
import           Lib.CallOption           (CallOption, applyCallOptions)
import           Lib.Logger
    ( LogEntry (..)
    , LogLevel (..)
    , LoggerConfig (..)
    , MonadLogger (..)
    , defaultLogger
    , newLogContext
    , renderLogEntry
    )
import           Network.Connection       (connectionApi)
import           Network.ConnectionAPI    (newConn)
import           Parser.Attoparsec        (parserApi)
import           Pipeline.Broadcasting    (broadcastingApi)
import           Pipeline.Streaming       (streamingApi)
import           Publish                  (defaultPublishConfig)
import           Publish.Config           (PublishConfig)
import           Queue.API                (QueueItem (QueueItem))
import           Queue.TransactionalQueue (newQueue)
import           State.Store
    ( ClientState
    , enqueue
    , newClientState
    , nextInbox
    , nextSid
    , pushPingAction
    , readServerInfo
    , readStatus
    , runClient
    , setConnectName
    , waitForClosed
    , waitForInitialConnection
    , waitForNotRunning
    )
import           State.Types
    ( ClientConfig (..)
    , ClientExitReason (..)
    , ClientStatus (..)
    , ConnectAttemptError (..)
    , ConnectError (..)
    , ConnectFailure (..)
    , TLSCertData
    , TLSPrivateKey
    , TLSPublicKey
    )
import           Subscription.Store
    ( SubscriptionStore
    , awaitNoTrackedExpiries
    , hasTrackedExpiries
    , newSubscriptionStore
    , register
    , startExpiryWorker
    , startWorkers
    , unregister
    )
import           Subscription.Types
    ( SubscribeConfig (..)
    , SubscriptionMeta (SubscriptionMeta)
    )
import qualified Types.Connect            as Connect
import qualified Types.Info               as Info
import qualified Types.Msg                as Msg
import           Types.Ping               (Ping (..))
import qualified Types.Pub                as Pub
import qualified Types.Sub                as Sub
import qualified Types.Unsub              as Unsub
import           Validators.Validators    (validate)

data ClientAuth = ClientAuthNone
                | ClientAuthToken AuthTokenData
                | ClientAuthUserPass UserPassData
                | ClientAuthNKey NKeyData
                | ClientAuthJWT JWTTokenData

data ClientOptions = ClientOptions
                       { optionConnectConfig        :: Connect.Connect
                       , optionAuth                 :: ClientAuth
                       , optionTlsCert              :: Maybe TLSCertData
                       , optionLoggerConfig         :: LoggerConfig
                       , optionConnectionAttempts   :: Int
                       , optionConnectTimeoutMicros :: Int
                       , optionCallbackConcurrency  :: Int
                       , optionBufferLimit          :: Int
                       , optionExitAction           :: ClientExitReason -> IO ()
                       , optionConnectOptions       :: [(String, Int)]
                       }

newClient :: [(String, Int)] -> [ConfigOption] -> IO (Either ConnectError Client)
newClient servers configOptions = do
  loggerConfig' <- defaultLogger
  ctx <- newLogContext
  let defaultOptions = applyCallOptions configOptions ClientOptions
        { optionConnectConfig = defaultConnect
        , optionAuth = ClientAuthNone
        , optionTlsCert = Nothing
        , optionLoggerConfig = loggerConfig'
        , optionConnectionAttempts = 5
        , optionConnectTimeoutMicros = 2 * 1000000
        , optionCallbackConcurrency = 1
        , optionBufferLimit = 4096
        , optionExitAction = const (pure ())
        , optionConnectOptions = servers
        }
      clientConfig =
        ClientConfig
          { connectionAttempts = optionConnectionAttempts defaultOptions
          , connectTimeoutMicros = optionConnectTimeoutMicros defaultOptions
          , callbackConcurrency = optionCallbackConcurrency defaultOptions
          , bufferLimit = optionBufferLimit defaultOptions
          , connectConfig = optionConnectConfig defaultOptions
          , loggerConfig = optionLoggerConfig defaultOptions
          , tlsCert = optionTlsCert defaultOptions
          , exitAction = optionExitAction defaultOptions
          , connectOptions = optionConnectOptions defaultOptions
          }
      configuredAuth = selectAuth (optionAuth defaultOptions)

  queue <- newQueue
  conn <- newConn connectionApi
  clientState <- newClientState clientConfig queue conn ctx
  store <- newSubscriptionStore

  setConnectName clientState (Connect.name (optionConnectConfig defaultOptions))
  logStaticConfiguration clientState defaultOptions

  startWorkers
    (callbackConcurrency clientConfig)
    store
    (do
        waitForClosed clientState
        awaitNoTrackedExpiries store)
    (handleCallbackError clientState)

  startExpiryWorker store $
    shouldStopExpiryWorker clientState store

  void . forkIO $
    runEngine
      connectionApi
      streamingApi
      broadcastingApi
      parserApi
      clientState
      store
      configuredAuth

  let client = Client
        { publish = \subject publishOptions -> do
            let cfg = applyCallOptions publishOptions defaultPublishConfig
            publishClient clientState store subject cfg
        , subscribe = \subject subscribeOptions callback -> do
            let cfg = applyCallOptions subscribeOptions defaultSubscribeConfig
            subscribeClient clientState store False subject cfg (toInternalCallback callback)
        , request = \subject subscribeOptions callback -> do
            let cfg = applyCallOptions subscribeOptions defaultSubscribeConfig
            subscribeClient clientState store True subject cfg (toInternalCallback callback)
        , unsubscribe = unsubscribeClient clientState store
        , newInbox = nextInbox clientState
        , ping = pingClient clientState
        , flush = flushClient clientState
        , reset = resetClient connectionApi clientState store
        , close = closeClient connectionApi clientState store
        }

  initialResult <- atomically (waitForInitialConnection clientState)
  pure (client <$ initialResult)

type ConfigOption = CallOption ClientOptions

withConnectName :: BS.ByteString -> ConfigOption
withConnectName name config =
  config
    { optionConnectConfig =
        (optionConnectConfig config) { Connect.name = Just name }
    }

withEcho :: Bool -> ConfigOption
withEcho enabled config =
  config
    { optionConnectConfig =
        (optionConnectConfig config) { Connect.echo = Just enabled }
    }

withAuthToken :: AuthTokenData -> ConfigOption
withAuthToken token config = config { optionAuth = ClientAuthToken token }

withUserPass :: UserPassData -> ConfigOption
withUserPass userPass config = config { optionAuth = ClientAuthUserPass userPass }

withNKey :: NKeyData -> ConfigOption
withNKey nkey config = config { optionAuth = ClientAuthNKey nkey }

withJWT :: JWTTokenData -> ConfigOption
withJWT jwt config = config { optionAuth = ClientAuthJWT jwt }

withTLSCert :: TLSCertData -> ConfigOption
withTLSCert cert config = config { optionTlsCert = Just cert }

withMinimumLogLevel :: LogLevel -> ConfigOption
withMinimumLogLevel minimumLogLevel config =
  config
    { optionLoggerConfig =
        (optionLoggerConfig config) { minLogLevel = minimumLogLevel }
    }

withLogAction :: (LogEntry -> IO ()) -> ConfigOption
withLogAction logAction config =
  config
    { optionLoggerConfig =
        (optionLoggerConfig config) { logFn = logAction }
    }

withConnectionAttempts :: Int -> ConfigOption
withConnectionAttempts attempts config =
  config { optionConnectionAttempts = max 1 attempts }

-- | Set the maximum time for INFO, TLS, CONNECT, and PONG negotiation on each
-- server attempt. Values below one microsecond are clamped to one.
withConnectTimeoutMicros :: Int -> ConfigOption
withConnectTimeoutMicros timeoutMicros config =
  config { optionConnectTimeoutMicros = max 1 timeoutMicros }

withCallbackConcurrency :: Int -> ConfigOption
withCallbackConcurrency concurrency config =
  config { optionCallbackConcurrency = concurrency }

withBufferLimit :: Int -> ConfigOption
withBufferLimit limit config =
  config { optionBufferLimit = max 1 limit }

withExitAction :: (ClientExitReason -> IO ()) -> ConfigOption
withExitAction action config = config { optionExitAction = action }

defaultConnect :: Connect.Connect
defaultConnect =
  Connect.Connect
    { Connect.verbose = False
    , Connect.pedantic = True
    , Connect.tls_required = False
    , Connect.auth_token = Nothing
    , Connect.user = Nothing
    , Connect.pass = Nothing
    , Connect.name = Nothing
    , Connect.lang = "haskell"
    , Connect.version = "0.1.0"
    , Connect.protocol = Nothing
    , Connect.echo = Just True
    , Connect.sig = Nothing
    , Connect.jwt = Nothing
    , Connect.nkey = Nothing
    , Connect.no_responders = Just True
    , Connect.headers = Just True
    }

defaultSubscribeConfig :: SubscribeConfig
defaultSubscribeConfig = SubscribeConfig Nothing Nothing

selectAuth :: ClientAuth -> Auth
selectAuth authSelection =
  case authSelection of
    ClientAuthNone ->
      AuthNone.auth
    ClientAuthToken token ->
      AuthToken.auth token
    ClientAuthUserPass userPass ->
      AuthUserPass.auth userPass
    ClientAuthNKey seed ->
      AuthNKey.auth seed
    ClientAuthJWT creds ->
      AuthJwt.auth creds

logStaticConfiguration :: ClientState -> ClientOptions -> IO ()
logStaticConfiguration client options =
  runClient client $ do
    case optionAuth options of
      ClientAuthNone ->
        logMessage Info "no authentication method provided"
      ClientAuthToken _ ->
        logMessage Info "using auth token"
      ClientAuthUserPass (user, _) ->
        logMessage Info ("using user/pass: " ++ show user)
      ClientAuthNKey _ ->
        logMessage Info "using nkey"
      ClientAuthJWT _ ->
        logMessage Info "using jwt"
    case optionTlsCert options of
      Nothing ->
        pure ()
      Just _ ->
        logMessage Info "using tls certificate"

handleCallbackError :: ClientState -> SomeException -> IO ()
handleCallbackError client err =
  runClient client $
    logMessage Error ("callback failed: " ++ displayException err)

shouldStopExpiryWorker :: ClientState -> SubscriptionStore -> IO Bool
shouldStopExpiryWorker client store = do
  status <- readStatus client
  tracked <- hasTrackedExpiries store
  pure $
    case status of
      Closed _ -> not tracked
      _        -> False

toInternalCallback :: (Maybe MsgView -> IO ()) -> Maybe Msg.Msg -> IO ()
toInternalCallback callback =
  callback . fmap toMsgView

toMsgView :: Msg.Msg -> MsgView
toMsgView msg =
  MsgView
    { subject = Msg.subject msg
    , sid = Msg.sid msg
    , replyTo = Msg.replyTo msg
    , payload = Msg.payload msg
    , headers = Msg.headers msg
    }

publishClient :: ClientState -> SubscriptionStore -> Msg.Subject -> PublishConfig -> IO ()
publishClient client store subject (payload, callback, headers, configuredReplyTo) = do
  runClient client $
    logMessage Debug ("publishing to subject: " ++ show subject)
  replyTo <- case callback of
    Nothing -> pure configuredReplyTo
    Just _  -> Just <$> maybe (nextInbox client) pure configuredReplyTo
  let publishMessage =
        Pub.Pub
          { Pub.subject = subject
          , Pub.payload = payload
          , Pub.replyTo = replyTo
          , Pub.headers = headers
          }
  shouldPublish <- canPublish client publishMessage
  when shouldPublish $ do
    case callback of
      Nothing ->
        pure ()
      Just replyCallback -> do
        case replyTo of
          Nothing ->
            pure ()
          Just replyInbox -> do
            sid <- nextSid client
            let meta =
                  SubscriptionMeta replyInbox Nothing True
            register store sid meta defaultSubscribeConfig replyCallback
            enqueue client $
              QueueItem
                Sub.Sub
                  { Sub.subject = replyInbox
                  , Sub.queueGroup = Nothing
                  , Sub.sid = sid
                  }
            enqueue client $
              QueueItem
                Unsub.Unsub
                  { Unsub.sid = sid
                  , Unsub.maxMsg = Just 1
                  }
    enqueue client (QueueItem publishMessage)

canPublish :: ClientState -> Pub.Pub -> IO Bool
canPublish client publishMessage =
  case validate publishMessage of
    Left reason -> do
      runClient client $
        logMessage Error ("dropping invalid publish: " ++ BC.unpack reason)
      pure False
    Right () -> do
      serverInfo <- readServerInfo client
      case serverInfo of
        Just info
          | Pub.messageSize publishMessage > Info.max_payload info -> do
              runClient client $
                logMessage Error
                  ("dropping publish: payload size "
                     ++ show (Pub.messageSize publishMessage)
                     ++ " exceeds server max_payload "
                     ++ show (Info.max_payload info))
              pure False
        _ ->
          pure True

subscribeClient :: ClientState -> SubscriptionStore -> Bool -> Msg.Subject -> SubscribeConfig -> (Maybe Msg.Msg -> IO ()) -> IO Msg.SID
subscribeClient client store isReply subject cfg callback = do
  runClient client $
    logMessage Debug ("subscribing to subject: " ++ show subject)
  sid <- nextSid client
  let queueGroup =
        subscribeQueueGroup cfg
      subscriptionMessage =
        Sub.Sub
          { Sub.subject = subject
          , Sub.queueGroup = queueGroup
          , Sub.sid = sid
          }
  case validate subscriptionMessage of
    Left reason ->
      runClient client $
        logMessage Error ("dropping invalid subscription: " ++ BC.unpack reason)
    Right () -> do
      let meta =
            SubscriptionMeta subject queueGroup isReply
      register store sid meta cfg callback
      enqueue client $
        QueueItem
          subscriptionMessage
      when isReply $
        enqueue client
          (QueueItem
            Unsub.Unsub
              { Unsub.sid = sid
              , Unsub.maxMsg = Just 1
              })
  pure sid

unsubscribeClient :: ClientState -> SubscriptionStore -> Msg.SID -> IO ()
unsubscribeClient client store sid = do
  runClient client $
    logMessage Debug ("unsubscribing SID: " ++ show sid)
  unregister store sid
  enqueue client $
    QueueItem
      Unsub.Unsub
        { Unsub.sid = sid
        , Unsub.maxMsg = Nothing
        }

pingClient :: ClientState -> IO () -> IO ()
pingClient client action = do
  runClient client $
    logMessage Debug "sending ping to server"
  pushPingAction client action
  enqueue client (QueueItem Ping)

flushClient :: ClientState -> IO ()
flushClient client = do
  ponged <- newEmptyTMVarIO
  pingClient client (atomically (void (tryPutTMVar ponged ())))
  atomically $
    readTMVar ponged `orElse` waitForNotRunning client
