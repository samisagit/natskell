{-# LANGUAGE OverloadedStrings #-}

-- | High-level client implementation for NATS.
module Client.Implementation
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
  , ServerError
  , serverErrorReason
  , ConnectError (..)
  , ConnectAttemptError (..)
  , ConnectFailure (..)
  ) where

import           Auth.Config              (authMethods, mergeAuth)
import qualified Auth.Jwt                 as AuthJwt
import qualified Auth.NKey                as AuthNKey
import qualified Auth.None                as AuthNone
import qualified Auth.Token               as AuthToken
import           Auth.Types
    ( Auth
    , AuthError (..)
    , AuthTokenData
    , AuthTokenHandler
    , JWTHandler
    , JWTTokenData
    , NKeyData
    , NKeyPublicKey
    , SignatureHandler
    , UserPassData
    , UserPassHandler
    )
import qualified Auth.UserPass            as AuthUserPass
import           Client.API
    ( Client (..)
    , CloseConfig (..)
    , FlushConfig (..)
    , Message (..)
    , NatsError (..)
    , PingConfig (..)
    , RequestConfig (..)
    , ResetConfig (..)
    , Subscription (..)
    , UnsubscribeConfig (..)
    )
import           Control.Concurrent       (forkIO)
import           Control.Concurrent.STM
import           Control.Exception
    ( SomeException
    , displayException
    , mask
    , onException
    )
import           Control.Monad            (forM_, void, when)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BC
import           Data.Char                (isAsciiUpper)
import           Data.Maybe               (fromMaybe)
import           Data.Time.Clock          (NominalDiffTime)
import           Data.Version             (showVersion)
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
import           Parser.Attoparsec        (parserApiWithMessageLimit)
import qualified Paths_natskell           as Package
import           Pipeline.Broadcasting    (broadcastingApi)
import           Pipeline.Streaming       (streamingApi)
import           Publish                  (defaultPublishConfig)
import           Publish.Config           (PublishConfig (..))
import           Queue.API                (QueueItem (QueueItem))
import           Queue.TransactionalQueue (newQueue)
import           State.Store
    ( ClientState
    , config
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
    , ServerError
    , TLSCertData
    , TLSConfig (..)
    , TLSPrivateKey
    , TLSPublicKey
    , defaultTLSConfig
    , serverErrorReason
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
    ( PendingLimits (..)
    , SubscribeConfig (..)
    , SubscriptionMeta (SubscriptionMeta)
    , defaultPendingLimits
    )
import qualified Types.Connect            as Connect
import qualified Types.Info               as Info
import qualified Types.Msg                as Msg
import           Types.Ping               (Ping (..))
import qualified Types.Pub                as Pub
import qualified Types.Sub                as Sub
import qualified Types.Unsub              as Unsub
import           Validators.Validators    (validate)

data ClientOptions = ClientOptions
                       { optionConnectConfig        :: Connect.Connect
                       , optionAuth                 :: Auth
                       , optionTlsConfig            :: Maybe TLSConfig
                       , optionLoggerConfig         :: LoggerConfig
                       , optionConnectionAttempts   :: Int
                       , optionConnectTimeoutMicros :: Int
                       , optionCallbackConcurrency  :: Int
                       , optionMessageLimit         :: Int
                       , optionPendingLimits        :: PendingLimits
                       , optionErrorHandler         :: NatsError -> IO ()
                       , optionExitAction           :: ClientExitReason -> IO ()
                       , optionConnectOptions       :: [(String, Int)]
                       }

-- | An opaque NATS server endpoint.
--
-- Keeping this representation private allows endpoint schemes and transports
-- to be added without changing the connection API.
data Server = Server String Int
  deriving (Eq, Show)

data ServerConfigError = EmptyServerHost
                       | InvalidServerPort Int
  deriving (Eq, Show)

-- | Construct a TCP NATS server endpoint.
server :: String -> Int -> Either ServerConfigError Server
server host port
  | null host = Left EmptyServerHost
  | port < 1 || port > 65535 = Left (InvalidServerPort port)
  | otherwise = Right (Server host port)

-- | Construct a server using the standard NATS port, 4222.
serverWithDefaultPort :: String -> Either ServerConfigError Server
serverWithDefaultPort host = server host 4222

serverHost :: Server -> String
serverHost (Server host _) = host

serverPort :: Server -> Int
serverPort (Server _ port) = port

-- | Connect to one of the configured NATS servers.
connect :: [Server] -> [ConfigOption] -> IO (Either ConnectError Client)
connect servers =
  newClient [(serverHost endpoint, serverPort endpoint) | endpoint <- servers]

-- | Compatibility connection entry point using raw @(host, port)@ tuples.
newClient :: [(String, Int)] -> [ConfigOption] -> IO (Either ConnectError Client)
newClient servers configOptions = do
  loggerConfig' <- defaultLogger
  ctx <- newLogContext
  let defaultOptions = applyCallOptions configOptions ClientOptions
        { optionConnectConfig = defaultConnect
        , optionAuth = AuthNone.auth
        , optionTlsConfig = Nothing
        , optionLoggerConfig = loggerConfig'
        , optionConnectionAttempts = 5
        , optionConnectTimeoutMicros = 2 * 1000000
        , optionCallbackConcurrency = 1
        , optionMessageLimit = 1024 * 1024
        , optionPendingLimits = defaultPendingLimits
        , optionErrorHandler = const (pure ())
        , optionExitAction = const (pure ())
        , optionConnectOptions = servers
        }
      clientConfig =
        ClientConfig
          { connectionAttempts = optionConnectionAttempts defaultOptions
          , connectTimeoutMicros = optionConnectTimeoutMicros defaultOptions
          , callbackConcurrency = optionCallbackConcurrency defaultOptions
          , messageLimit = optionMessageLimit defaultOptions
          , connectConfig = optionConnectConfig defaultOptions
          , loggerConfig = optionLoggerConfig defaultOptions
          , tlsConfig = optionTlsConfig defaultOptions
          , exitAction = optionExitAction defaultOptions
          , connectOptions = optionConnectOptions defaultOptions
          }
      configuredAuth = optionAuth defaultOptions

  queue <- newQueue
  conn <- newConn connectionApi
  clientState <- newClientState clientConfig queue conn ctx
  store <-
    newSubscriptionStore
      (optionPendingLimits defaultOptions)
      (handleSlowConsumer clientState (optionErrorHandler defaultOptions))

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
      (parserApiWithMessageLimit (messageLimit clientConfig))
      clientState
      store
      configuredAuth

  let client = Client
        { publish = \subject payload publishOptions -> do
            let cfg = applyCallOptions publishOptions defaultPublishConfig
            publishClient clientState subject payload cfg
        , subscribe = \subject subscribeOptions callback -> do
            let cfg = applyCallOptions subscribeOptions defaultSubscribeConfig
            subscribeClient clientState store False subject cfg callback
        , subscribeOnce = \subject subscribeOptions callback -> do
            let cfg = applyCallOptions subscribeOptions defaultSubscribeConfig
            subscribeClient clientState store True subject cfg callback
        , request = \subject payload requestOptions -> do
            let cfg = applyCallOptions requestOptions defaultRequestConfig
            requestClient clientState store subject payload cfg
        , unsubscribe = \subscription options ->
            case applyCallOptions options UnsubscribeConfig of
              UnsubscribeConfig -> unsubscribeClient clientState store subscription
        , newInbox = nextInbox clientState
        , ping = \options ->
            case applyCallOptions options PingConfig of
              PingConfig -> flushClient clientState
        , flush = \options ->
            case applyCallOptions options FlushConfig of
              FlushConfig -> flushClient clientState
        , reset = \options ->
            case applyCallOptions options ResetConfig of
              ResetConfig -> resetClient connectionApi clientState store
        , close = \options ->
            case applyCallOptions options CloseConfig of
              CloseConfig -> closeClient connectionApi clientState store
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
withAuthToken token = addAuth (AuthToken.auth token)

-- | Fetch a token for every connection and reconnection attempt.
withAuthTokenHandler :: AuthTokenHandler -> ConfigOption
withAuthTokenHandler handler = addAuth (AuthToken.authHandler handler)

withUserPass :: UserPassData -> ConfigOption
withUserPass userPass = addAuth (AuthUserPass.auth userPass)

-- | Fetch a username and password for every connection and reconnection attempt.
withUserPassHandler :: UserPassHandler -> ConfigOption
withUserPassHandler handler = addAuth (AuthUserPass.authHandler handler)

withNKey :: NKeyData -> ConfigOption
withNKey seed = addAuth (AuthNKey.auth seed)

-- | Authenticate with a public NKey and a handler that signs the server nonce.
-- The handler returns the raw 64-byte Ed25519 signature; natskell performs the
-- protocol's base64url encoding.
withNKeyHandler :: NKeyPublicKey -> SignatureHandler -> ConfigOption
withNKeyHandler publicKey handler = addAuth (AuthNKey.authHandler publicKey handler)

withJWT :: JWTTokenData -> ConfigOption
withJWT creds = addAuth (AuthJwt.auth creds)

-- | Fetch a user JWT and sign the server nonce for every connection attempt.
withJWTHandlers :: JWTHandler -> SignatureHandler -> ConfigOption
withJWTHandlers jwtHandler signatureHandler =
  addAuth (AuthJwt.authHandlers jwtHandler signatureHandler)

addAuth :: Auth -> ConfigOption
addAuth auth config =
  config { optionAuth = mergeAuth (optionAuth config) auth }

-- | Require TLS using the operating system trust store.
withTLS :: ConfigOption
withTLS = modifyTLSConfig id

withTLSCert :: TLSCertData -> ConfigOption
withTLSCert cert =
  modifyTLSConfig $ \tls -> tls { tlsClientCertificate = Just cert }

-- | Trust a PEM-encoded root certificate for this client. Once configured,
-- these roots replace the operating-system trust store for the connection.
withTLSRootCA :: BS.ByteString -> ConfigOption
withTLSRootCA root =
  modifyTLSConfig $ \tls ->
    tls { tlsRootCertificates = tlsRootCertificates tls ++ [root] }

-- | Override the host name used for certificate verification and SNI.
withTLSServerName :: String -> ConfigOption
withTLSServerName serverName =
  modifyTLSConfig $ \tls -> tls { tlsServerName = Just serverName }

-- | Disable server certificate verification. This is unsafe and should only
-- be used when the peer is trusted by some mechanism outside TLS.
withTLSInsecure :: ConfigOption
withTLSInsecure =
  modifyTLSConfig $ \tls -> tls { tlsInsecure = True }

modifyTLSConfig :: (TLSConfig -> TLSConfig) -> ConfigOption
modifyTLSConfig update config =
  config
    { optionTlsConfig =
        Just (update (fromMaybe defaultTLSConfig (optionTlsConfig config)))
    }

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

-- | Set the largest encoded message body this client accepts. For messages
-- with headers, the encoded header block and payload both count toward the
-- limit. Outbound messages are also constrained by the server's max_payload.
withMessageLimit :: Int -> ConfigOption
withMessageLimit limit config =
  config { optionMessageLimit = max 1 limit }

-- | Bound callback deliveries pending across the entire client. The first
-- limit is a message count and the second is encoded message bytes.
withPendingDeliveryLimits :: Int -> Int -> ConfigOption
withPendingDeliveryLimits maximumMessages maximumBytes config =
  config
    { optionPendingLimits =
        PendingLimits
          { pendingMessageLimit = max 1 maximumMessages
          , pendingByteLimit = max 1 maximumBytes
          }
    }

-- | Receive asynchronous client errors such as slow-consumer notifications.
-- The handler runs on the callback worker pool, never the socket reader.
withErrorHandler :: (NatsError -> IO ()) -> ConfigOption
withErrorHandler handler config =
  config { optionErrorHandler = handler }

-- | Compatibility alias for 'withMessageLimit'.
withBufferLimit :: Int -> ConfigOption
withBufferLimit = withMessageLimit

{-# DEPRECATED withBufferLimit "Use withMessageLimit instead." #-}

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
    , Connect.version = BC.pack (showVersion Package.version)
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

defaultRequestConfig :: RequestConfig
defaultRequestConfig = RequestConfig 2 Nothing

logStaticConfiguration :: ClientState -> ClientOptions -> IO ()
logStaticConfiguration client options =
  runClient client $ do
    case authMethods (optionAuth options) of
      [] -> logMessage Info "no authentication method provided"
      methods -> forM_ methods $ \method ->
        logMessage Info ("using " ++ method)
    case optionTlsConfig options of
      Nothing ->
        pure ()
      Just tls -> do
        logMessage Info "using tls"
        when (tlsInsecure tls) $
          logMessage Warn "tls certificate verification is disabled"

handleCallbackError :: ClientState -> SomeException -> IO ()
handleCallbackError client err =
  runClient client $
    logMessage Error ("callback failed: " ++ displayException err)

handleSlowConsumer :: ClientState -> (NatsError -> IO ()) -> IO ()
handleSlowConsumer client handler = do
  runClient client $
    logMessage Error "slow consumer: global pending delivery limit reached"
  handler NatsSlowConsumer

shouldStopExpiryWorker :: ClientState -> SubscriptionStore -> IO Bool
shouldStopExpiryWorker client store = do
  status <- readStatus client
  tracked <- hasTrackedExpiries store
  pure $
    case status of
      Closed _ -> not tracked
      _        -> False

toMessage :: Msg.Msg -> Message
toMessage msg =
  Message
    { subject = Msg.subject msg
    , sid = Msg.sid msg
    , replyTo = Msg.replyTo msg
    , payload = fromMaybe BS.empty (Msg.payload msg)
    , headers = Msg.headers msg
    }

publishClient
  :: ClientState
  -> Msg.Subject
  -> Msg.Payload
  -> PublishConfig
  -> IO (Either NatsError ())
publishClient client subject messagePayload cfg = do
  runClient client $
    logMessage Debug ("publishing to subject: " ++ show subject)
  let publishMessage =
        Pub.Pub
          { Pub.subject = subject
          , Pub.payload =
              if BS.null messagePayload then Nothing else Just messagePayload
          , Pub.replyTo = publishReplyTo cfg
          , Pub.headers = publishHeaders cfg
          }
  validation <- canPublish client publishMessage
  case validation of
    Left err -> pure (Left err)
    Right () -> do
      enqueue client (QueueItem publishMessage)
      pure (Right ())

canPublish :: ClientState -> Pub.Pub -> IO (Either NatsError ())
canPublish client publishMessage =
  case validate publishMessage of
    Left reason -> do
      runClient client $
        logMessage Error ("rejecting invalid publish: " ++ BC.unpack reason)
      pure (Left (NatsValidationError reason))
    Right () -> do
      statusResult <- runningResult client
      case statusResult of
        Left err -> pure (Left err)
        Right () -> do
          serverInfo <- readServerInfo client
          let actual = Pub.messageSize publishMessage
              clientMaximum = messageLimit (config client)
              maximumSize =
                maybe
                  clientMaximum
                  (min clientMaximum . Info.max_payload)
                  serverInfo
          if actual > maximumSize
            then do
              runClient client $
                logMessage Error
                  ( "rejecting publish: message size "
                      ++ show actual
                      ++ " exceeds effective limit "
                      ++ show maximumSize
                  )
              pure (Left (NatsPayloadTooLarge actual maximumSize))
            else
              pure (Right ())

subscribeClient
  :: ClientState
  -> SubscriptionStore
  -> Bool
  -> Msg.Subject
  -> SubscribeConfig
  -> (Message -> IO ())
  -> IO (Either NatsError Subscription)
subscribeClient client store isReply subject cfg callback =
  subscribeRawClient client store isReply subject cfg (maybe (pure ()) (callback . toMessage))

subscribeRawClient
  :: ClientState
  -> SubscriptionStore
  -> Bool
  -> Msg.Subject
  -> SubscribeConfig
  -> (Maybe Msg.Msg -> IO ())
  -> IO (Either NatsError Subscription)
subscribeRawClient client store isReply subject cfg callback = do
  subscribeRawClientWithOverflow
    client
    store
    isReply
    subject
    cfg
    callback
    (pure ())

subscribeRawClientWithOverflow
  :: ClientState
  -> SubscriptionStore
  -> Bool
  -> Msg.Subject
  -> SubscribeConfig
  -> (Maybe Msg.Msg -> IO ())
  -> IO ()
  -> IO (Either NatsError Subscription)
subscribeRawClientWithOverflow client store isReply subject cfg callback onDropped = do
  runClient client $
    logMessage Debug ("subscribing to subject: " ++ show subject)
  statusResult <- runningResult client
  let queueGroup = subscribeQueueGroup cfg
  sid <- nextSid client
  let
      subscriptionMessage =
        Sub.Sub
          { Sub.subject = subject
          , Sub.queueGroup = queueGroup
          , Sub.sid = sid
          }
  case validate subscriptionMessage of
    Left reason -> do
      runClient client $
        logMessage Error ("rejecting invalid subscription: " ++ BC.unpack reason)
      pure (Left (NatsValidationError reason))
    Right () -> case statusResult of
      Left err -> pure (Left err)
      Right () -> do
        let meta =
              SubscriptionMeta subject queueGroup isReply
        register store sid meta cfg callback onDropped
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
        pure (Right (Subscription sid))

requestClient
  :: ClientState
  -> SubscriptionStore
  -> Msg.Subject
  -> Msg.Payload
  -> RequestConfig
  -> IO (Either NatsError Message)
requestClient client store requestSubject requestPayload cfg =
  mask $ \restore -> do
    response <- newEmptyTMVarIO
    deadline <- registerDelay (durationMicros (requestTimeout cfg))
    inbox <- nextInbox client
    let subscriptionConfig = SubscribeConfig Nothing Nothing
        deliver Nothing = atomically (void (tryPutTMVar response (Left NatsRequestTimedOut)))
        deliver (Just msg) =
          atomically . void . tryPutTMVar response $
            let message = toMessage msg
            in if isNoResponders message
                 then Left NatsNoResponders
                 else Right message
        rejectSlowConsumer =
          atomically (void (tryPutTMVar response (Left NatsSlowConsumer)))
    subscriptionResult <-
      subscribeRawClientWithOverflow
        client
        store
        True
        inbox
        subscriptionConfig
        deliver
        rejectSlowConsumer
    case subscriptionResult of
      Left err -> pure (Left err)
      Right subscription -> do
        let publishConfig = PublishConfig (requestHeaders cfg) (Just inbox)
            cleanup = void (unsubscribeClient client store subscription)
        publishResult <- publishClient client requestSubject requestPayload publishConfig
        case publishResult of
          Left err -> cleanup >> pure (Left err)
          Right () -> do
            result <- restore (awaitRequest client deadline response) `onException` cleanup
            cleanup
            pure result

awaitRequest
  :: ClientState
  -> TVar Bool
  -> TMVar (Either NatsError Message)
  -> IO (Either NatsError Message)
awaitRequest client deadline response = do
  outcome <- atomically $
    (Just <$> readTMVar response)
      `orElse` (do
        expired <- readTVar deadline
        check expired
        pure (Just (Left NatsRequestTimedOut)))
      `orElse` (Nothing <$ waitForNotRunning client)
  case outcome of
    Just result -> pure result
    Nothing     -> do
      status <- readStatus client
      pure (Left (closedError status))

durationMicros :: NominalDiffTime -> Int
durationMicros duration =
  fromInteger (min (toInteger (maxBound :: Int)) micros)
  where
    micros = max 0 (floor (realToFrac duration * (1000000 :: Double)))

isNoResponders :: Message -> Bool
isNoResponders message =
  case headers message of
    Nothing -> False
    Just messageHeaders ->
      any isNoRespondersHeader messageHeaders
  where
    isNoRespondersHeader (name, value) =
      BC.map toAsciiLower name == "status" && value == "503"
    toAsciiLower byte
      | isAsciiUpper byte = toEnum (fromEnum byte + 32)
      | otherwise = byte

unsubscribeClient
  :: ClientState
  -> SubscriptionStore
  -> Subscription
  -> IO (Either NatsError ())
unsubscribeClient client store (Subscription sid) = do
  runClient client $
    logMessage Debug ("unsubscribing SID: " ++ show sid)
  unregister store sid
  statusResult <- runningResult client
  case statusResult of
    Left err -> pure (Left err)
    Right () -> do
      enqueue client $
        QueueItem
          Unsub.Unsub
            { Unsub.sid = sid
            , Unsub.maxMsg = Nothing
            }
      pure (Right ())

pingClient :: ClientState -> IO () -> IO ()
pingClient client action = do
  runClient client $
    logMessage Debug "sending ping to server"
  pushPingAction client action
  enqueue client (QueueItem Ping)

flushClient :: ClientState -> IO (Either NatsError ())
flushClient client = do
  statusResult <- runningResult client
  case statusResult of
    Left err -> pure (Left err)
    Right () -> do
      ponged <- newEmptyTMVarIO
      pingClient client (atomically (void (tryPutTMVar ponged ())))
      outcome <- atomically $
        (True <$ readTMVar ponged)
          `orElse` (False <$ waitForNotRunning client)
      if outcome
        then pure (Right ())
        else do
          status <- readStatus client
          pure (Left (closedError status))

runningResult :: ClientState -> IO (Either NatsError ())
runningResult client = do
  status <- readStatus client
  pure $
    case status of
      Running        -> Right ()
      Closing reason -> Left (NatsConnectionClosed reason)
      Closed reason  -> Left (NatsConnectionClosed reason)

closedError :: ClientStatus -> NatsError
closedError status =
  case status of
    Closing reason -> NatsConnectionClosed reason
    Closed reason  -> NatsConnectionClosed reason
    Running        -> NatsConnectionClosed ExitResetRequested
