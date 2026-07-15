{-# LANGUAGE LambdaCase        #-}
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
import           Control.Concurrent
    ( forkIOWithUnmask
    , killThread
    , myThreadId
    )
import           Control.Concurrent.STM
import           Control.Exception
    ( SomeException
    , finally
    , mask
    , mask_
    , onException
    )
import           Control.Monad            (forM_, void, when)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BC
import           Data.Char                (isAsciiUpper)
import           Data.Maybe               (fromMaybe, isJust)
import           Data.Time.Clock          (NominalDiffTime)
import           Data.Version             (showVersion)
import           Engine
    ( closeClient
    , interruptConnection
    , resetClient
    , runEngine
    )
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
import           Network.ConnectionAPI    (ConnectionAPI, newConn)
import qualified Network.ConnectionAPI    as Connection
import           Parser.Attoparsec        (parserApiWithMessageLimit)
import qualified Paths_natskell           as Package
import           Pipeline.Broadcasting    (broadcastingApi)
import           Pipeline.Streaming       (streamingApi)
import           Publish                  (defaultPublishConfig)
import           Publish.Config           (PublishConfig (..))
import           Queue.API
    ( QueueItem (QueueBatch, QueueConnectionScoped, QueueItem, QueuePayloadBound)
    , TryEnqueueResult (..)
    )
import           Queue.TransactionalQueue (newQueue)
import           State.Store
    ( ClientState
    , PingResult (..)
    , PublishEnqueueResult (..)
    , config
    , enqueue
    , enqueueOnGenerationTracked
    , enqueuePublishOnConnected
    , enqueuePublishOnConnectedGenerationTracked
    , isManagedThread
    , markClosed
    , newClientState
    , nextInbox
    , nextSid
    , readConnectionGeneration
    , readStatus
    , registerManagedThread
    , registerPingWaiterAndEnqueue
    , runClient
    , setClosing
    , setConnectName
    , startCallbackWorker
    , stopCallbackWorker
    , tryEnqueue
    , tryEnqueueOnGeneration
    , unregisterManagedThread
    , waitForCallbackWorker
    , waitForClosed
    , waitForConnected
    , waitForConnectionGenerationLoss
    , waitForInitialConnection
    , waitForNotRunning
    , withSubscriptionGate
    )
import           State.Types
    ( ClientConfig (..)
    , ClientExitReason (..)
    , ConnectAttemptError (..)
    , ConnectError (..)
    , ConnectFailure (..)
    , ConnectionEvent (..)
    , ConnectionState (..)
    , ServerError
    , ServerErrorKind (..)
    , TLSCertData
    , TLSConfig (..)
    , TLSPrivateKey
    , TLSPublicKey
    , defaultTLSConfig
    , serverErrorKind
    , serverErrorReason
    )
import           Subscription.Store
    ( SubscriptionStore
    , awaitCallbackDrain
    , awaitNoTrackedExpiries
    , closeStore
    , enqueueControl
    , hasTrackedExpiries
    , newSubscriptionStore
    , register
    , registerWithDispatchHooks
    , startExpiryWorker
    , startWorkers
    , unregister
    )
import           Subscription.Types
    ( PendingLimits (..)
    , SubscribeConfig (..)
    , SubscriptionKind (..)
    , SubscriptionMeta (SubscriptionMeta)
    , defaultPendingLimits
    , isOneShotSubscription
    )
import           System.Timeout           (timeout)
import qualified Types.Connect            as Connect
import qualified Types.Msg                as Msg
import           Types.Ping               (Ping (..))
import qualified Types.Pub                as Pub
import qualified Types.Sub                as Sub
import qualified Types.Unsub              as Unsub
import           Validators.Validators    (validate)

data ClientOptions = ClientOptions
                       { optionConnectConfig :: Connect.Connect
                       , optionAuth :: Auth
                       , optionTlsConfig :: Maybe TLSConfig
                       , optionLoggerConfig :: LoggerConfig
                       , optionConnectionAttempts :: Int
                       , optionConnectTimeoutMicros :: Int
                       , optionCallbackConcurrency :: Int
                       , optionMessageLimit :: Int
                       , optionPendingLimits :: PendingLimits
                       , optionErrorHandler :: NatsError -> IO ()
                       , optionServerErrorHandler :: ServerError -> IO ()
                       , optionConnectionEventHandler :: ConnectionEvent -> IO ()
                       , optionExitAction :: ClientExitReason -> IO ()
                       , optionConnectOptions :: [(String, Int)]
                       }

data SubscriptionQueueResult = SubscriptionQueued | SubscriptionReconnect

data UnsubscribeQueueResult = UnsubscribeQueued | UnsubscribeReconnect | UnsubscribeReset

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
newClient servers configOptions = mask $ \restoreInitialWait -> do
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
        , optionServerErrorHandler = const (pure ())
        , optionConnectionEventHandler = const (pure ())
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
          , serverErrorHandler = optionServerErrorHandler defaultOptions
          , connectionEventHandler = optionConnectionEventHandler defaultOptions
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

  callbackThreads <- startWorkers
    (callbackConcurrency clientConfig)
    store
    (do
        waitForClosed clientState
        awaitCallbackDrain store
        awaitNoTrackedExpiries store)
    (handleCallbackError clientState)

  expiryThread <- startExpiryWorker store $
    shouldStopExpiryWorker clientState store

  mapM_ (registerManagedThread clientState) (expiryThread : callbackThreads)
  callbackWorker <- startCallbackWorker clientState

  engineDone <- newEmptyTMVarIO
  engineThread <- forkIOWithUnmask $ \unmask ->
    do
      threadId <- myThreadId
      registerManagedThread clientState threadId
      unmask
        ( runEngine
            connectionApi
            streamingApi
            broadcastingApi
            (parserApiWithMessageLimit (messageLimit clientConfig))
            clientState
            store
            configuredAuth
        )
        `finally` do
          unregisterManagedThread clientState threadId
          atomically (void (tryPutTMVar engineDone ()))

  let auxiliaryThreads = expiryThread : callbackThreads
      stopAuxiliaryThreads excluded =
        mapM_ killThread (filter (/= excluded) auxiliaryThreads)
      finishClose excluded = do
        atomically (readTMVar engineDone)
        Connection.close connectionApi conn
        atomically (awaitCallbackDrain store)
        stopAuxiliaryThreads excluded
        atomically (waitForCallbackWorker callbackWorker)
      scheduleFinish excluded = do
        _ <- forkIOWithUnmask $ \unmask -> unmask (finishClose excluded)
        pure ()
      closeAndFinish = mask $ \restore -> do
        current <- myThreadId
        closeClient connectionApi clientState store
          `onException` scheduleFinish current
        managed <- isManagedThread clientState current
        if managed
          then scheduleFinish current
          else restore (finishClose current) `onException` scheduleFinish current

  let client = Client
        { publish = \subject payload publishOptions -> do
            let cfg = applyCallOptions publishOptions defaultPublishConfig
            publishClient
              clientState
              (enqueueControl store . optionErrorHandler defaultOptions)
              subject
              payload
              cfg
        , subscribe = \subject subscribeOptions callback -> do
            let cfg = applyCallOptions subscribeOptions defaultSubscribeConfig
            subscribeClient clientState store StandardSubscription subject cfg callback
        , subscribeOnce = \subject subscribeOptions callback -> do
            let cfg = applyCallOptions subscribeOptions defaultSubscribeConfig
            subscribeClient clientState store OneShotSubscription subject cfg callback
        , request = \subject payload requestOptions -> do
            let cfg = applyCallOptions requestOptions defaultRequestConfig
            requestClient clientState store subject payload cfg
        , unsubscribe = \subscription options ->
            case applyCallOptions options UnsubscribeConfig of
              UnsubscribeConfig -> unsubscribeClient clientState store subscription
        , newInbox = nextInbox clientState
        , ping = \options ->
            case applyCallOptions options defaultPingConfig of
              PingConfig -> flushClient connectionApi clientState defaultRoundTripTimeout
              PingConfigTimeout timeoutSeconds ->
                flushClient connectionApi clientState timeoutSeconds
        , flush = \options ->
            case applyCallOptions options defaultFlushConfig of
              FlushConfig -> flushClient connectionApi clientState defaultRoundTripTimeout
              FlushConfigTimeout timeoutSeconds ->
                flushClient connectionApi clientState timeoutSeconds
        , connectionState = readStatus clientState
        , reset = \options ->
            case applyCallOptions options ResetConfig of
              ResetConfig -> resetClient connectionApi clientState store
        , close = \options ->
            case applyCallOptions options CloseConfig of
              CloseConfig -> closeAndFinish
        }

  let abortInitialWait = do
        setClosing clientState ExitClosedByUser
        closeStore store
        void (interruptConnection connectionApi clientState)
        killThread engineThread
        atomically (readTMVar engineDone)
        Connection.close connectionApi conn
        void (markClosed clientState ExitClosedByUser)
        stopCallbackWorker callbackWorker
        current <- myThreadId
        stopAuxiliaryThreads current
  initialResult <-
    restoreInitialWait
      (atomically $
        waitForInitialConnection clientState
          `orElse` (Left (ConnectAttemptsExhausted []) <$ readTMVar engineDone))
      `onException` abortInitialWait
  case initialResult of
    Left err -> do
      atomically (readTMVar engineDone)
      Connection.close connectionApi conn
      stopCallbackWorker callbackWorker
      current <- myThreadId
      stopAuxiliaryThreads current
      pure (Left err)
    Right () -> pure (Right client)

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

-- | Set the total time budget for TCP acquisition, INFO, TLS, CONNECT, PONG,
-- and reconnect resubscription/readiness on each server attempt. Values below
-- one microsecond are clamped to one.
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

-- | Receive protocol @-ERR@ values on a bounded, dedicated serial worker.
-- Protocol processing, including PONG handling, does not wait for the handler.
withServerErrorHandler :: (ServerError -> IO ()) -> ConfigOption
withServerErrorHandler handler config =
  config { optionServerErrorHandler = handler }

-- | Receive completed disconnect, reconnect, and close transitions on the
-- same serial callback worker as server errors. Lifecycle delivery is reserved
-- independently from the bounded server-error backlog.
withConnectionEventHandler :: (ConnectionEvent -> IO ()) -> ConfigOption
withConnectionEventHandler handler config =
  config { optionConnectionEventHandler = handler }

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

defaultPingConfig :: PingConfig
defaultPingConfig = PingConfig

defaultFlushConfig :: FlushConfig
defaultFlushConfig = FlushConfig

defaultRoundTripTimeout :: NominalDiffTime
defaultRoundTripTimeout = 2

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
handleCallbackError client _ =
  runClient client $
    logMessage Error "callback failed"

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
      ConnectionClosed _ -> not tracked
      _                  -> False

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
  -> (NatsError -> IO ())
  -> Msg.Subject
  -> Msg.Payload
  -> PublishConfig
  -> IO (Either NatsError ())
publishClient client errorHandler subject messagePayload cfg = do
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
      actualSize = Pub.messageSize publishMessage
      queuedPublish =
        QueuePayloadBound
          actualSize
          (errorHandler . NatsPayloadTooLarge actualSize)
          (QueueItem publishMessage)
  validation <- validatePublish client publishMessage
  case validation of
    Left err -> pure (Left err)
    Right () -> do
      enqueueResult <- enqueuePublishOnConnected client actualSize queuedPublish
      publishEnqueueResult client actualSize enqueueResult

validatePublish :: ClientState -> Pub.Pub -> IO (Either NatsError ())
validatePublish client publishMessage =
  case validate publishMessage of
    Left reason -> do
      runClient client $
        logMessage Error ("rejecting invalid publish: " ++ BC.unpack reason)
      pure (Left (NatsValidationError reason))
    Right () -> do
      let actualSize = Pub.messageSize publishMessage
          clientMaximum = messageLimit (config client)
      if actualSize > clientMaximum
        then rejectPayloadTooLarge client actualSize clientMaximum
        else pure (Right ())

publishEnqueueResult
  :: ClientState
  -> Int
  -> PublishEnqueueResult
  -> IO (Either NatsError ())
publishEnqueueResult client actualSize enqueueResult =
  case enqueueResult of
    PublishEnqueued _ -> pure (Right ())
    PublishTooLarge maximumSize ->
      rejectPayloadTooLarge client actualSize maximumSize
    PublishConnectionClosed reason ->
      pure (Left (NatsConnectionClosed reason))

rejectPayloadTooLarge :: ClientState -> Int -> Int -> IO (Either NatsError a)
rejectPayloadTooLarge client actualSize maximumSize = do
  runClient client $
    logMessage Error
      ( "rejecting publish: message size "
          ++ show actualSize
          ++ " exceeds effective limit "
          ++ show maximumSize
      )
  pure (Left (NatsPayloadTooLarge actualSize maximumSize))

subscribeClient
  :: ClientState
  -> SubscriptionStore
  -> SubscriptionKind
  -> Msg.Subject
  -> SubscribeConfig
  -> (Message -> IO ())
  -> IO (Either NatsError Subscription)
subscribeClient client store subscriptionKind subject cfg callback =
  subscribeRawClient client store subscriptionKind subject cfg (maybe (pure ()) (callback . toMessage))

subscribeRawClient
  :: ClientState
  -> SubscriptionStore
  -> SubscriptionKind
  -> Msg.Subject
  -> SubscribeConfig
  -> (Maybe Msg.Msg -> IO ())
  -> IO (Either NatsError Subscription)
subscribeRawClient client store subscriptionKind subject cfg callback = do
  subscribeRawClientWithOverflow
    client
    store
    subscriptionKind
    subject
    cfg
    callback
    (pure ())

subscribeRawClientWithOverflow
  :: ClientState
  -> SubscriptionStore
  -> SubscriptionKind
  -> Msg.Subject
  -> SubscribeConfig
  -> (Maybe Msg.Msg -> IO ())
  -> IO ()
  -> IO (Either NatsError Subscription)
subscribeRawClientWithOverflow client store subscriptionKind subject cfg callback onDropped =
  mask $ \restore -> do
    runClient client $
      logMessage Debug ("subscribing to subject: " ++ show subject)
    let queueGroup = subscribeQueueGroup cfg
    sid <- nextSid client
    registeredGeneration <- newEmptyTMVarIO
    committedGeneration <- newEmptyTMVarIO
    let subscriptionMessage =
          Sub.Sub
            { Sub.subject = subject
            , Sub.queueGroup = queueGroup
            , Sub.sid = sid
            }
        meta = SubscriptionMeta subject queueGroup subscriptionKind
        subscription = Subscription sid
        cleanup =
          cleanupResumableSubscription
            client
            store
            registeredGeneration
            committedGeneration
            subscription
        deliver maybeMessage = do
          case maybeMessage of
            Nothing -> cleanup
            Just _  -> pure ()
          callback maybeMessage
        commands =
          QueueItem subscriptionMessage :
            [ QueueItem
                Unsub.Unsub
                  { Unsub.sid = sid
                  , Unsub.maxMsg = Just 1
                  }
            | isOneShotSubscription meta
            ]
        queuedCommands = QueueConnectionScoped (QueueBatch commands)
    case validate subscriptionMessage of
      Left reason -> do
        runClient client $
          logMessage Error ("rejecting invalid subscription: " ++ BC.unpack reason)
        pure (Left (NatsValidationError reason))
      Right () -> do
        let waitForServerRegistration = do
              statusResult <- runningResult client
              case statusResult of
                Left err -> cleanup >> pure (Left err)
                Right () -> pure (Right subscription)
            acquireGate = do
              statusResult <- runningResult client
              case statusResult of
                Left err -> pure (Left err)
                Right () -> do
                  gated <- withSubscriptionGate client $ do
                    status <- readStatus client
                    case status of
                      ConnectionConnected -> do
                        generation <- readConnectionGeneration client
                        atomically (void (tryPutTMVar registeredGeneration generation))
                        register store sid meta cfg deliver onDropped
                        enqueueResult <-
                          enqueueOnGenerationTracked
                            client
                            generation
                            committedGeneration
                            queuedCommands
                        case enqueueResult of
                          Right _ ->
                            pure (Just (Right SubscriptionQueued))
                          Left _ -> do
                            nextStatus <- readStatus client
                            case nextStatus of
                              ConnectionClosing reason ->
                                pure (Just (Left (NatsConnectionClosed reason)))
                              ConnectionClosed reason ->
                                pure (Just (Left (NatsConnectionClosed reason)))
                              _ ->
                                pure (Just (Right SubscriptionReconnect))
                      _ -> pure Nothing
                  maybe acquireGate pure gated
            subscribeOperation = do
              queueResult <- acquireGate
              case queueResult of
                Left err                    -> cleanup >> pure (Left err)
                Right SubscriptionQueued -> do
                  runClient client $
                    logMessage Debug "subscription commands queued"
                  pure (Right subscription)
                Right SubscriptionReconnect -> waitForServerRegistration
        restore subscribeOperation `onException` cleanup

requestClient
  :: ClientState
  -> SubscriptionStore
  -> Msg.Subject
  -> Msg.Payload
  -> RequestConfig
  -> IO (Either NatsError Message)
requestClient client store requestSubject requestPayload cfg = do
  timedResult <-
    timeout
      (durationMicros (requestTimeout cfg))
      (requestBeforeDeadline client store requestSubject requestPayload cfg)
  pure (fromMaybe (Left NatsRequestTimedOut) timedResult)

requestBeforeDeadline
  :: ClientState
  -> SubscriptionStore
  -> Msg.Subject
  -> Msg.Payload
  -> RequestConfig
  -> IO (Either NatsError Message)
requestBeforeDeadline client store requestSubject requestPayload cfg =
  mask $ \restore -> do
    response <- newEmptyTMVarIO
    accepted <- newEmptyTMVarIO
    committedGeneration <- newEmptyTMVarIO
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
          void (tryPutTMVar response (Left NatsSlowConsumer))
    sid <- nextSid client
    let subscription = Subscription sid
        subscriptionMessage =
          Sub.Sub
            { Sub.subject = inbox
            , Sub.queueGroup = Nothing
            , Sub.sid = sid
            }
        unsubscribeMessage =
          Unsub.Unsub
            { Unsub.sid = sid
            , Unsub.maxMsg = Just 1
            }
        publishMessage =
          Pub.Pub
            { Pub.subject = requestSubject
            , Pub.payload =
                if BS.null requestPayload then Nothing else Just requestPayload
            , Pub.replyTo = Just inbox
            , Pub.headers = requestHeaders cfg
            }
        commands = QueueConnectionScoped . QueueBatch $
          [ QueueItem subscriptionMessage
          , QueueItem unsubscribeMessage
          , QueueItem publishMessage
          ]
        meta = SubscriptionMeta inbox Nothing RequestReplySubscription
    case validate subscriptionMessage of
      Left reason -> pure (Left (NatsValidationError reason))
      Right () -> do
        publishValidation <- validatePublish client publishMessage
        case publishValidation of
          Left err -> pure (Left err)
          Right () -> do
            registerWithDispatchHooks
              store
              sid
              meta
              subscriptionConfig
              (void (tryPutTMVar accepted ()))
              rejectSlowConsumer
              deliver
              (pure ())
            let cleanupCommitted = do
                  committed <- atomically (tryReadTMVar committedGeneration)
                  case committed of
                    Nothing -> unregister store sid
                    Just generation ->
                      cleanupRequestSubscription client store generation subscription
            let publishSize = Pub.messageSize publishMessage
            queued <- restore
              ( enqueuePublishOnConnectedGenerationTracked
                  client
                  committedGeneration
                  publishSize
                  commands
              )
              `onException` cleanupCommitted
            case queued of
              PublishConnectionClosed reason -> do
                unregister store sid
                pure (Left (NatsConnectionClosed reason))
              PublishTooLarge maximumSize -> do
                unregister store sid
                rejectPayloadTooLarge client publishSize maximumSize
              PublishEnqueued generation -> do
                let cleanup =
                      cleanupRequestSubscription client store generation subscription
                result <- restore (awaitRequest client generation accepted response)
                  `onException` cleanup
                cleanup
                pure result

awaitRequest
  :: ClientState
  -> Int
  -> TMVar ()
  -> TMVar (Either NatsError Message)
  -> IO (Either NatsError Message)
awaitRequest client generation accepted response =
  atomically $
    readTMVar response
      `orElse`
        do
          acceptedReply <- tryReadTMVar accepted
          case acceptedReply of
            Just () -> retry
            Nothing ->
              Left . NatsConnectionClosed
                <$> waitForConnectionGenerationLoss client generation

durationMicros :: NominalDiffTime -> Int
durationMicros duration =
  fromInteger (min (toInteger (maxBound :: Int)) micros)
  where
    micros = max 0 (floor (toRational duration * 1000000))

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
unsubscribeClient client store (Subscription sid) =
  mask $ \restore -> do
    runClient client $
      logMessage Debug ("unsubscribing SID: " ++ show sid)
    let command = QueueConnectionScoped . QueueItem $
          Unsub.Unsub
            { Unsub.sid = sid
            , Unsub.maxMsg = Nothing
            }
        unregisterAndTry = do
          unregister store sid
          tryEnqueue client command >>= \case
            TryEnqueued    -> pure (Right UnsubscribeQueued)
            TryQueueClosed -> pure (Right UnsubscribeReconnect)
            TryQueueFull   -> pure (Right UnsubscribeReset)
    queueResult <- withSubscriptionGate client $ do
      status <- readStatus client
      case status of
        ConnectionConnected -> do
          unregister store sid
          enqueueResult <-
            restore (enqueue client command)
              `onException` interruptConnection connectionApi client
          case enqueueResult of
            Right () -> pure (Right UnsubscribeQueued)
            Left _ -> do
              nextStatus <- readStatus client
              case nextStatus of
                ConnectionClosing reason ->
                  pure (Left (NatsConnectionClosed reason))
                ConnectionClosed reason ->
                  pure (Left (NatsConnectionClosed reason))
                _ -> pure (Right UnsubscribeReconnect)
        ConnectionConnecting -> unregisterAndTry
        ConnectionReconnecting -> unregisterAndTry
        ConnectionClosing reason -> do
          unregister store sid
          pure (Left (NatsConnectionClosed reason))
        ConnectionClosed reason -> do
          unregister store sid
          pure (Left (NatsConnectionClosed reason))
    case queueResult of
      Left err                    -> pure (Left err)
      Right UnsubscribeQueued    -> pure (Right ())
      Right UnsubscribeReconnect -> pure (Right ())
      Right UnsubscribeReset     -> do
        interruptConnection connectionApi client
        pure (Right ())

cleanupResumableSubscription
  :: ClientState
  -> SubscriptionStore
  -> TMVar Int
  -> TMVar Int
  -> Subscription
  -> IO ()
cleanupResumableSubscription client store registeredGeneration committedGeneration (Subscription sid) =
  mask_ $ do
    resetRequired <- withSubscriptionGate client $ do
      unregister store sid
      status <- readStatus client
      case status of
        ConnectionConnected -> do
          generation <- readConnectionGeneration client
          registered <- atomically (tryReadTMVar registeredGeneration)
          committed <- atomically (tryReadTMVar committedGeneration)
          let serverMayKnow =
                isJust committed || maybe False (< generation) registered
          if serverMayKnow
            then do
              result <- tryEnqueueOnGeneration client generation unsubscribeCommand
              pure (result == TryQueueFull)
            else pure False
        _ -> pure False
    when resetRequired (void (interruptConnection connectionApi client))
  where
    unsubscribeCommand = QueueConnectionScoped . QueueItem $
      Unsub.Unsub { Unsub.sid = sid, Unsub.maxMsg = Nothing }

cleanupRequestSubscription :: ClientState -> SubscriptionStore -> Int -> Subscription -> IO ()
cleanupRequestSubscription client store generation (Subscription sid) = mask_ $ do
  unregister store sid
  result <- tryEnqueueOnGeneration client generation . QueueConnectionScoped . QueueItem $
    Unsub.Unsub { Unsub.sid = sid, Unsub.maxMsg = Nothing }
  case result of
    TryEnqueued    -> pure ()
    TryQueueClosed -> pure ()
    TryQueueFull   -> void (interruptConnection connectionApi client)

flushClient :: ConnectionAPI -> ClientState -> NominalDiffTime -> IO (Either NatsError ())
flushClient connectionApi' client timeoutSeconds =
  mask $ \restore -> do
    let reset = interruptConnection connectionApi' client
    timedResult <-
      restore (timeout (durationMicros timeoutSeconds) (awaitPong client))
        `onException` reset
    case timedResult of
      Nothing     -> reset >> pure (Left NatsRequestTimedOut)
      Just result -> pure result
  where
    awaitPong client' = do
      waiter <- newEmptyTMVarIO
      registered <- registerPingWaiterAndEnqueue
        client'
        waiter
        (QueueConnectionScoped (QueueItem Ping))
      case registered of
        Left reason -> pure (Left (NatsConnectionClosed reason))
        Right _ -> do
          result <- atomically $
            (Just <$> readTMVar waiter)
              `orElse` (Nothing <$ waitForNotRunning client')
          case result of
            Just PingReceived -> pure (Right ())
            Just PingConnectionLost ->
              Left . closedError <$> readStatus client'
            Nothing -> do
              status <- readStatus client'
              pure (Left (closedError status))

runningResult :: ClientState -> IO (Either NatsError ())
runningResult client = do
  result <- atomically (waitForConnected client)
  pure $
    case result of
      Left reason -> Left (NatsConnectionClosed reason)
      Right ()    -> Right ()

closedError :: ConnectionState -> NatsError
closedError status =
  case status of
    ConnectionClosing reason -> NatsConnectionClosed reason
    ConnectionClosed reason  -> NatsConnectionClosed reason
    _                        -> NatsConnectionClosed ExitResetRequested
