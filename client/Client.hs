-- | High-level client API for NATS.
module Client (
  Client,
  MsgView (..),
  newClient,
  publish,
  subscribe,
  unsubscribe,
  ping,
  flush,
  reset,
  Client.close,
  withPayload,
  withReplyCallback,
  withHeaders,
  withConnectName,
  withAuthToken,
  withUserPass,
  withNKey,
  withJWT,
  withTLSCert,
  withLoggerConfig,
  withConnectionAttempts,
  withExitAction,
  withSubscriptionExpiry,
  LoggerConfig (..),
  LogLevel (..),
  LogEntry (..),
  renderLogEntry,
  AuthTokenData,
  UserPassData,
  NKeyData,
  JWTTokenData,
  TLSPublicKey,
  TLSPrivateKey,
  TLSCertData,
  SubscribeOpts,
  ClientExitReason (..),
  ) where

import           Client.Auth
    ( buildConnectPayload
    , defaultConnect
    , logAuthMethod
    , logTlsConfig
    )
import           Client.Lifecycle
    ( markClosed
    , setLifecycleClosing
    , setServerInfo
    , updateLogContextFromInfo
    , waitForClosed
    , waitForClosing
    , waitForServerInfo
    )
import           Client.Runtime
    ( readConfig
    , readConfigState
    , runClient
    , writeToClientQueue
    )
import           Client.Subscription
    ( awaitSubscriptionGC
    , cancelExpiredSubscriptions
    , msgRouterM
    , nextInbox
    , subscribeInternal
    , unsubscribeInternal
    )
import           Client.Types
    ( Client (..)
    , emptySubscriptionState
    , initialConfigState
    )
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BC
import           Data.Maybe
import           Lib.CallOption
import           Lib.Logger
import qualified Lib.Parser                as Parser
import           MSGView
import           Network.API
import           Network.Connection
import           Options
import           Parsers.Parsers
import qualified Pipeline.Broadcasting.API as B
import qualified Pipeline.Streaming.API    as S
import           Queue.API
import           Queue.TransactionalQueue  (QueueItem (..))
import           Types
import qualified Types.Connect             as Connect (Connect (..))
import qualified Types.Err                 as E
import qualified Types.Info                as I
import           Types.Ping
import           Types.Pong
import qualified Types.Pub                 as P
import           WaitGroup

-- | newClient creates a new client with optional overrides to default settings.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- servers = [(\"127.0.0.1\", 4222)]
-- opts = [withConnectName \"example-client\"]
-- client <- newClient servers opts
-- @
newClient :: [(String, Int)] -> [ConfigOpts] -> IO Client
newClient servers conOpts = do
  dl <- defaultLogger
  ctx <- newLogContext
  let defaultConfig = applyCallOptions conOpts Config {
    connectConfig = defaultConnect
    , auth = None
    , tlsCert = Nothing
    , loggerConfig = dl
    , connectionAttempts = 5
    , exitAction = const (return ())
    , connectOptions = servers
  }
  updateLogContext ctx (\c -> c { lcConnectName = fmap BC.unpack (Connect.name (connectConfig defaultConfig)) })

  c <- newConn

  client <- liftIO $ Client'
    <$> newQ
    <*> newTVarIO emptySubscriptionState
    <*> newTQueueIO
    <*> newTVarIO initialSIDCounter
    <*> (newTVarIO =<< Nuid.newNuidIO)
    <*> newTVarIO (initialConfigState defaultConfig)
    <*> newTVarIO (connectionAttempts defaultConfig)
    <*> newTVarIO Running
    <*> pure c
    <*> pure ctx

  runClient client $ do
    logAuthMethod (auth defaultConfig)
    logTlsConfig (tlsCert defaultConfig)
  forkIO $ retryLoop client

  -- ensure that the server info is initialized
  atomically $ waitForServerInfo client `orElse` waitForClosed client

  return client

acquireTransport :: Client -> IO (Either String (Conn, (String, Int)))
acquireTransport client = do
  attempts <- readTVarIO (connectionAttempts' client)
  cfg <- readConfig client
  case connectOptions cfg of
    []             -> return (Left "No servers provided")
    xs -> do
      let (host, port) = cycle xs !! attempts
      updateLogContext (logContext client) (\ctx -> ctx { lcServer = Just (host ++ ":" ++ show port) })
      transportResult <- Network.Connection.openTcpTransport host port
      case transportResult of
        Left err -> return (Left err)
        Right transport -> do
          pointTransport (conn client) transport
          return . Right $ (conn client, (host, port))

decomTransport ::  Either String (Conn, (String, Int)) -> IO ()
decomTransport (Left _)       = return ()
decomTransport (Right (h, _)) = closeConnection h

withTransport :: Client -> (Either String (Conn, (String, Int)) -> IO a) -> IO a
withTransport client = bracket
    (acquireTransport client)
    decomTransport

withRetry :: Client -> Int -> IO () -> IO ()
withRetry c 0 _ = do
  runClient c $ logMessage Info "retries exhausted; exiting"
  cfg <- readConfig c
  result <- atomically $ markClosed c (ExitRetriesExhausted Nothing)
  maybe (return ()) (exitAction cfg) result
withRetry c x action = do
  state <- readTVarIO (lifecycle c)
  case state of
    Closing _ -> do
      runClient c $ logMessage Info "client closing; skipping retries"
      cfg <- readConfig c
      result <- atomically $ markClosed c ExitClosedByUser
      maybe (return ()) (exitAction cfg) result
    Closed _ -> return ()
    Running -> do
      action
      runClient c $ logMessage Info "retrying client connection"
      atomically $ modifyTVar (connectionAttempts' c) (+1)
      withRetry c (x-1) action

withSubscriptionGC :: Client -> IO () -> IO ()
withSubscriptionGC client action = bracket
  (forkIO . forever . cancelExpiredSubscriptions $ client)
  (\tid -> do
    awaitSubscriptionGC client
    killThread tid
  )
  (const action)

retryLoop :: Client -> IO ()
retryLoop client = do
  cfg <- readConfig client
  let attemptsLeft = connectionAttempts cfg
  withRetry client attemptsLeft $ do
    Queue.API.open (queue client)
    openConnection (conn client)
    withSubscriptionGC client $ do
      withTransport client $ \transportResult -> do
        case transportResult of
          (Left e) -> runClient client . logMessage Error $ ("connection attempt failed: " ++ show e)
          (Right (connection, (host, _port))) -> do
            initResult <- initializeConnection client connection host
            case initResult of
              Left err ->
                runClient client . logMessage Error $ ("connection initialization failed: " ++ err)
              Right () -> do
                runClient client $ do
                  wgs <- liftIO $ newWaitGroup 1
                  wgb <- liftIO $ newWaitGroup 1
                  logMessage Debug "starting client streaming threads"
                  liftIO . void . forkWaitGroup wgb $ do
                    runClient client $ B.run (queue client) connection
                    runClient client $ logMessage Debug "broadcasting thread exited"
                  liftIO . void . forkWaitGroup wgs $ do
                    runClient client $ S.run connection genericParse (router client)
                    runClient client $ logMessage Debug "streaming thread exited"
                  wg <- liftIO $ newWaitGroup 2
                  liftIO . void . forkWaitGroup wg $ do
                    wait wgs
                    -- close broadcasting
                    liftIO $ Queue.API.close (queue client)
                  liftIO . void . forkWaitGroup wg $ do
                    wait wgb
                    -- close streaming
                    closeReader connection
                  liftIO $ wait wg
                  logMessage Debug "streaming threads exited; closing connection"

initializeConnection :: Client -> Conn -> String -> IO (Either String ())
initializeConnection client h host = do
  infoResult <- readInitialInfo client h
  case infoResult of
    Left err -> return (Left err)
    Right (info, rest) -> do
      cfg <- readConfig client
      let tlsRequested = isJust (tlsCert cfg) || Connect.tls_required (connectConfig cfg)
          tlsRequired = fromMaybe False (I.tls_required info)
          transportOptions = TransportOptions
            { transportHost = host
            , transportTlsRequired = tlsRequired
            , transportTlsRequested = tlsRequested
            , transportTlsCert = tlsCert cfg
            , transportInitialBytes = rest
            }
      transportResult <- configureTransport h transportOptions
      case transportResult of
        Left transportErr -> return (Left transportErr)
        Right () -> do
          runClient client $ routerM client (ParsedInfo info)
          return (Right ())

readInitialInfo :: Client -> Conn -> IO (Either String (I.Info, BS.ByteString))
readInitialInfo client h =
  go mempty
  where
    go acc = do
      result <- readData h 4096
      case result of
        Left err -> return (Left err)
        Right chunk ->
          if BS.null chunk
            then do
              runClient client $ logMessage Debug "no data read, waiting"
              threadDelay 100000
              go acc
            else do
              let bs = acc <> chunk
              case genericParse bs of
                Left err ->
                  case Parser.solveErr err (BS.length bs) of
                    Parser.SuggestPull ->
                      go bs
                    Parser.SuggestDrop n _ ->
                      go (BS.drop n bs)
                Right (ParsedInfo info, rest) ->
                  return (Right (info, rest))
                Right (ParsedErr err, _) ->
                  return (Left ("server error before INFO: " ++ show err))
                Right (_, rest) ->
                  go rest

-- | publish sends a message to the NATS server.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- publish client \"updates\" [withPayload \"hello\"]
-- publish client \"service.echo\"
--   [ withPayload \"hello\"
--   , withReplyCallback print
--   ]
-- @
publish :: Client -> Subject -> [PubOptions -> PubOptions] -> IO ()
publish client subject opts = do
  runClient client . logMessage Debug $ ("publishing to subject: " ++ show subject)
  let (payload, callback, headers) = applyCallOptions opts (Nothing, Nothing, Nothing)

  case callback of
    Just cb -> do
      inbox <- nextInbox client
      request client inbox [] cb
      let msg = P.Pub {
        P.subject = subject,
        P.payload = payload,
        P.replyTo = Just inbox,
        P.headers = headers
      }
      writeToClientQueue client (QueueItem msg)
    Nothing -> do
      let msg = P.Pub {
        P.subject = subject,
        P.payload = payload,
        P.replyTo = Nothing,
        P.headers = headers
      }
      writeToClientQueue client (QueueItem msg)

-- | subscribe is used to subscribe to a subject on the NATS server.
-- Options can be passed to tune subscription behaviour (e.g., reply expiry).
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- subscribe client \"events.created\" [] print
-- @
subscribe :: Client -> Subject -> [SubscribeOpts] -> (Maybe MsgView -> IO ()) -> IO SID
subscribe = subscribeInternal False

-- | request is used to subscribe to a reply subject on the NATS server.
-- Options can be passed to tune subscription behaviour (e.g., reply expiry).
request :: Client -> Subject -> [SubscribeOpts] -> (Maybe MsgView -> IO ()) -> IO SID
request = subscribeInternal True

-- | unsubscribe is used to unsubscribe from a subject on the NATS server.
--
-- __Examples:__
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- sid <- subscribe client \"events.created\" [] print
-- unsubscribe client sid
-- @
unsubscribe :: Client -> SID -> IO ()
unsubscribe = unsubscribeInternal

-- | ping is used to send a ping message to the NATS server.
ping :: Client -> IO () -> IO ()
ping client action = do
  runClient client $ logMessage Debug "sending ping to server"
  atomically $ writeTQueue (pings client) action
  writeToClientQueue client (QueueItem Ping)

flush :: Client -> IO ()
flush client = do
  ponged <- newEmptyTMVarIO
  ping client (atomically (void (tryPutTMVar ponged ())))
  atomically $ readTMVar ponged `orElse` waitForClosing client

shutdownClient :: Client -> ClientExitReason -> String -> IO ()
shutdownClient client reason message = do
  atomically $ setLifecycleClosing client reason
  runClient client $ logMessage Info message
  Queue.API.close $ queue client
  closeReader (conn client)
  atomically $ waitForClosed client

-- | close is used to close the connection to the NATS server.
close :: Client -> IO ()
close client = shutdownClient client ExitClosedByUser "closing client connection"

-- | reset is used to abort the current connection and trigger lifecycle closure without attempting graceful drain.
reset :: Client -> IO ()
reset client = shutdownClient client ExitResetRequested "resetting client connection"

-- internal handlers

router :: Client -> ParsedMessage -> IO ()
router client = runClient client . routerM client

routerM :: Client -> ParsedMessage -> AppM ()
routerM client msg = do
  case msg of
    ParsedMsg a  -> do
      logMessage Debug $ "routing MSG: " ++ show a
      msgRouterM client a
    ParsedInfo i -> do
      logMessage Debug $ "routing INFO: " ++ show i
      liftIO $ do
        atomically (setServerInfo client i)
        updateLogContextFromInfo client i
        connect client
    ParsedPing _ -> do
      logMessage Debug "routing PING"
      liftIO $ pong client
    ParsedPong _ -> do
      logMessage Debug "routing PONG"
      liftIO $ runPingAction (pings client)
    ParsedOk   okMsg -> logMessage Debug $ "routing OK: " ++ show okMsg
    ParsedErr err -> do
      logMessage Debug $ "routing ERR: " ++ show err
      case E.isFatal err of
        True  -> do
          logMessage Error $ "fatal server error: " ++ show err
          liftIO $ do
            atomically $ setLifecycleClosing client (ExitServerError err)
            reset client
        False -> logMessage Warn $ "server error: " ++ show err

runPingAction :: TQueue (IO ()) -> IO ()
runPingAction actionsQueue = do
  action <- atomically $ tryReadTQueue actionsQueue
  fromMaybe (return ()) action

connect :: Client -> IO ()
connect client = do
  runClient client $ logMessage Debug "connecting to NATS server"
  cfg <- readConfig client
  info <- cfgServerInfo <$> readConfigState client
  let nonce = info >>= I.nonce
  let infoTlsRequired = fromMaybe False (info >>= I.tls_required)
      tlsActive = infoTlsRequired || isJust (tlsCert cfg) || Connect.tls_required (connectConfig cfg)
  let dat = buildConnectPayload cfg nonce tlsActive
  writeToClientQueue client (QueueItem dat)

pong :: Client -> IO ()
pong client = writeToClientQueue client (QueueItem Pong)
