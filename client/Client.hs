{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

-- | High-level client API for NATS.
module Client (
  Client,
  MsgView (..),
  newClient,
  publish,
  subscribe,
  unsubscribe,
  ping,
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

import qualified Auth.NKey                 as NKey
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BC
import           Data.Heap
    ( MinHeap
    , empty
    , insert
    , view
    , viewHead
    )
import           Data.Map                  (Map, delete, insert, lookup)
import           Data.Maybe
import           Data.Time.Clock
import           Lib.CallOption
import           Lib.Logger
import qualified Lib.Parser                as Parser
import           MSGView
import           Network.API
import           Network.Connection
import qualified Network.Simple.TCP        as TCP
import qualified Network.Socket            as NS
import qualified Network.TLS               as TLS
import qualified Nuid
import           Options
import           Parsers.Parsers
import qualified Pipeline.Broadcasting.API as B
import qualified Pipeline.Streaming.API    as S
import           Prelude                   hiding (lookup)
import           Queue.API
import           Queue.TransactionalQueue
import           Sid
    ( SIDCounter
    , initialSIDCounter
    , nextSID
    )
import           Types
import qualified Types.Connect             as Connect (Connect (..))
import qualified Types.Err                 as E
import qualified Types.Info                as I
import qualified Types.Msg                 as M
import           Types.Ping
import           Types.Pong
import qualified Types.Pub                 as P
import qualified Types.Sub                 as Sub
import qualified Types.Unsub               as Unsub
import           WaitGroup

data SubscriptionHeapItem = SubscriptionHeapItem
                              { sid'   :: SID
                              , expiry :: UTCTime
                              }

instance Eq SubscriptionHeapItem where
  a == b = sid' a == sid' b && expiry a == expiry b

instance Ord SubscriptionHeapItem where
  a `compare` b = expiry a `compare` expiry b

type SubscriptionHeap = MinHeap SubscriptionHeapItem

data SubscriptionState = SubscriptionState
                           { subscriptionCallbacks :: Map SID (Maybe M.Msg -> IO ())
                           , subscriptionExpiries :: SubscriptionHeap
                           }

emptySubscriptionState :: SubscriptionState
emptySubscriptionState = SubscriptionState mempty empty

data ConfigState = ConfigState
                     { cfgConfig     :: Config
                     , cfgServerInfo :: Maybe I.Info
                     }

initialConfigState :: Config -> ConfigState
initialConfigState cfg = ConfigState cfg Nothing

data LifecycleState = Running
                    | Closing ClientExitReason
                    | Closed ClientExitReason

readConfigState :: Client -> IO ConfigState
readConfigState client = readTVarIO (configState client)

readConfig :: Client -> IO Config
readConfig = fmap cfgConfig . readConfigState

updateLogContextFromInfo :: Client -> I.Info -> IO ()
updateLogContextFromInfo client info = do
  updateLogContext (logContext client) (\ctx -> ctx
    { lcClientId = I.client_id info
    })

setServerInfo :: Client -> I.Info -> STM ()
setServerInfo client info =
  modifyTVar' (configState client) (\state -> state { cfgServerInfo = Just info })

setLifecycleClosing :: Client -> ClientExitReason -> STM ()
setLifecycleClosing client reason =
  modifyTVar' (lifecycle client) $ \case
    Closed result -> Closed result
    Closing r     -> Closing r
    Running       -> Closing reason

markClosed :: Client -> ClientExitReason -> STM (Maybe ClientExitReason)
markClosed client fallbackReason = do
  state <- readTVar (lifecycle client)
  case state of
    Closed _ -> return Nothing
    Closing reason -> do
      writeTVar (lifecycle client) (Closed reason)
      return (Just reason)
    Running -> do
      writeTVar (lifecycle client) (Closed fallbackReason)
      return (Just fallbackReason)

waitForClosed :: Client -> STM ()
waitForClosed client = do
  state <- readTVar (lifecycle client)
  case state of
    Closed _ -> return ()
    _        -> retry

waitForServerInfo :: Client -> STM ()
waitForServerInfo client = do
  cfgState <- readTVar (configState client)
  case cfgServerInfo cfgState of
    Just _  -> return ()
    Nothing -> retry

waitForClosing :: Client -> STM ()
waitForClosing client = do
  state <- readTVar (lifecycle client)
  case state of
    Running -> retry
    _       -> return ()

cancelExpiredSubscriptions :: Client -> IO ()
cancelExpiredSubscriptions client = do
  runClient client $ logMessage Debug "running subscription GC"
  now <- getCurrentTime
  action <- atomically $ do
    state <- readTVar (subscriptions client)
    let viewed = viewHead (subscriptionExpiries state)
    case viewed of
      Nothing -> retry
      Just item ->
        if now < expiry item
          then return Nothing
          else do
            case view (subscriptionExpiries state) of
              Nothing -> return Nothing
              Just (_, heapTail) -> do
                let callback = lookup (sid' item) (subscriptionCallbacks state)
                    newCallbacks = maybe (subscriptionCallbacks state) (const (delete (sid' item) (subscriptionCallbacks state))) callback
                    newState = state { subscriptionCallbacks = newCallbacks, subscriptionExpiries = heapTail }
                writeTVar (subscriptions client) newState
                return callback
  runClient client $ case action of
    Just a -> liftIO $ a Nothing
    Nothing  -> do
      liftIO $ threadDelay 1000000 -- 1 second
      return ()

awaitSubscriptionGC :: Client -> IO ()
awaitSubscriptionGC client = do
  atomically $ do
    subs <- readTVar (subscriptions client)
    when (isJust (viewHead (subscriptionExpiries subs))) retry

-- | Client is used to interact with the NATS server.
data Client = Client'
                { queue               :: Q QueueItem
                , subscriptions       :: TVar SubscriptionState
                , pings               :: TVar [IO ()]
                , sidCounter          :: TVar SIDCounter
                , inboxNuid           :: TVar Nuid.Nuid
                , configState         :: TVar ConfigState
                , connectionAttempts' :: TVar Int
                , lifecycle           :: TVar LifecycleState
                , conn                :: Conn
                , logContext          :: TVar LogContext
                }

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
    <*> newTVarIO []
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
      socketResult <- try @SomeException (defaultSocket host port)
      case socketResult of
        Left e -> return (Left (show e))
        Right socket -> do
          pointTransport (conn client) (tcpTransport socket)
          return . Right $ (conn client, (host, port))

decomTransport ::  Either String (Conn, (String, Int)) -> IO ()
decomTransport (Left _)       = return ()
decomTransport (Right (h, _)) = Network.Connection.close h

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
    Network.Connection.open (conn client)
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
                  liftIO . void . forkIO $ do
                    runClient client $ B.run (queue client) connection
                    runClient client $ logMessage Debug "broadcasting thread exited"
                    done wgb
                  liftIO . void . forkIO $ do
                    runClient client $ S.run connection genericParse (router client)
                    runClient client $ logMessage Debug "streaming thread exited"
                    done wgs
                  wg <- liftIO $ newWaitGroup 2
                  liftIO . void . forkIO $ do
                    wait wgs
                    -- close broadcasting
                    liftIO $ Queue.API.close (queue client)
                    done wg
                  liftIO . void . forkIO $ do
                    wait wgb
                    -- close streaming
                    closeReader connection
                    done wg
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
          useTls = tlsRequested || tlsRequired
      if useTls
        then do
          runClient client $ logMessage Info "negotiating TLS"
          paramsResult <- buildTlsParams host (tlsCert cfg)
          case paramsResult of
            Left err -> return (Left err)
            Right params -> do
              tlsResult <- Network.Connection.upgradeToTLS h params
              case tlsResult of
                Left tlsErr -> return (Left tlsErr)
                Right () -> do
                  unless (BS.null rest)
                    (runClient client $ logMessage Warn "dropping pre-TLS plaintext")
                  runClient client $ routerM client (ParsedInfo info)
                  return (Right ())
        else do
          bufferRead h rest
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

buildTlsParams :: String -> Maybe TLSCertData -> IO (Either String TLS.ClientParams)
buildTlsParams host tlsConfig = do
  let base = TLS.defaultParamsClient host (BC.pack host)
      hooks = TLS.clientHooks base
      shared = TLS.clientShared base
      acceptAny = hooks { TLS.onServerCertificate = \_ _ _ _ -> return [] }
  case tlsConfig of
    Just (certPem, keyPem) ->
      case TLS.credentialLoadX509FromMemory certPem keyPem of
        Left err -> return (Left err)
        Right cred ->
          let hooks' = acceptAny { TLS.onCertificateRequest = \_ -> return (Just cred) }
              shared' = shared { TLS.sharedCredentials = TLS.Credentials [cred] }
          in return (Right base { TLS.clientHooks = hooks', TLS.clientShared = shared' })
    Nothing -> return (Right base { TLS.clientHooks = acceptAny })

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
      flushThen client (writeToClientQueue client (QueueItem msg))
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
subscribe = subscribe' False

-- | request is used to subscribe to a reply subject on the NATS server.
-- Options can be passed to tune subscription behaviour (e.g., reply expiry).
request :: Client -> Subject -> [SubscribeOpts] -> (Maybe MsgView -> IO ()) -> IO SID
request = subscribe' True

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
unsubscribe client sid = do
  runClient client . logMessage Debug $ ("unsubscribing SID: " ++ show sid)
  atomically $ modifyTVar' (subscriptions client) (\s -> s { subscriptionCallbacks = delete sid (subscriptionCallbacks s) })
  let unsub = Unsub.Unsub {
    Unsub.sid = sid,
    Unsub.maxMsg = Nothing
  }

  writeToClientQueue client (QueueItem unsub)

-- | ping is used to send a ping message to the NATS server.
ping :: Client -> IO () -> IO ()
ping client action = do
  runClient client $ logMessage Debug "sending ping to server"
  atomically $ modifyTVar' (pings client) (action :)
  writeToClientQueue client (QueueItem Ping)

flush :: Client -> IO ()
flush client = do
  ponged <- newEmptyTMVarIO
  ping client (atomically (void (tryPutTMVar ponged ())))
  atomically $ readTMVar ponged `orElse` waitForClosing client

flushThen :: Client -> IO () -> IO ()
flushThen client action = do
  void . forkIO $ do
    flush client
    action

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

runClient :: Client -> AppM a -> IO a
runClient client action = do
  cfg <- readConfig client
  runWithLogger (loggerConfig cfg) (logContext client) action

defaultSocket :: String -> Int -> IO NS.Socket
defaultSocket host port = do
  (sock, _) <- TCP.connectSock host $ show port
  NS.setSocketOption sock NS.NoDelay 1
  NS.setSocketOption sock NS.Cork 0
  NS.setSocketOption sock NS.RecvBuffer 1
  NS.setSocketOption sock NS.SendBuffer 1
  return sock

subscribe' :: Bool -> Client -> Subject -> [SubscribeOpts] -> (Maybe MsgView -> IO ()) -> IO SID
subscribe' isReply client subject opts callback = do
  runClient client . logMessage Debug $ ("subscribing to subject: " ++ show subject)
  sid <- nextSid client
  let subConfig = applyCallOptions opts defaultSubscribeConfig
  let cb = if isReply
        then \m -> do
          callback (fmap transformMsg m)
          unsubscribe client sid
        else callback . fmap transformMsg
  let replyExpiry = subscriptionExpiry subConfig
  expiry <- addUTCTime replyExpiry <$> getCurrentTime
  atomically $ do
    modifyTVar' (subscriptions client) $ \subs ->
      let callbacks' = Data.Map.insert sid cb (subscriptionCallbacks subs)
          expiries' = if isReply
            then Data.Heap.insert (SubscriptionHeapItem sid expiry) (subscriptionExpiries subs)
            else subscriptionExpiries subs
      in subs { subscriptionCallbacks = callbacks', subscriptionExpiries = expiries' }
  let sub = Sub.Sub {
    Sub.subject = subject,
    Sub.queueGroup = Nothing,
    Sub.sid = sid
  }

  writeToClientQueue client (QueueItem sub)
  return sid

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
      liftIO $ sequenceActions (pings client)
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

logAuthMethod :: Auth -> AppM ()
logAuthMethod auth = case auth of
  None               -> logMessage Info "no authentication method provided"
  AuthToken _        -> logMessage Info "using auth token"
  UserPass (user, _) -> logMessage Info $ "using user/pass: " ++ show user
  NKey _             -> logMessage Info "using nkey"
  JWT _              -> logMessage Info "using jwt"

logTlsConfig :: Maybe TLSCertData -> AppM ()
logTlsConfig tlsConfig = case tlsConfig of
  Nothing -> pure ()
  Just _  -> logMessage Info "using tls certificate"

sequenceActions :: TVar [IO ()] -> IO ()
sequenceActions actionsVar = do
  actions <- atomically $ do
    as <- readTVar actionsVar
    writeTVar actionsVar []
    return as
  sequence_ actions

msgRouterM :: Client -> M.Msg -> AppM ()
msgRouterM client msg = do
  let sid = M.sid msg
  callbacks <- liftIO $ subscriptionCallbacks <$> readTVarIO (subscriptions client)
  case lookup sid callbacks of
    Just callback -> do
      logMessage Debug $ "running callback for SID: " ++ show sid
      liftIO . callback $ Just msg
    Nothing       -> logMessage Error $ "callback missing for SID: " ++ show sid

nextSid :: Client -> IO SID
nextSid client = atomically $ do
  counter <- readTVar (sidCounter client)
  let (sid, counter') = nextSID counter
  writeTVar (sidCounter client) counter'
  return sid

nextInbox :: Client -> IO Subject
nextInbox client = atomically $ do
  nuid <- readTVar (inboxNuid client)
  let (token, nuid') = Nuid.nextNuid nuid
      inbox = BS.append inboxPrefix token
  writeTVar (inboxNuid client) nuid'
  return inbox

inboxPrefix :: Subject
inboxPrefix = "_INBOX."

defaultConnect = Connect.Connect{
  Connect.verbose = False,
  Connect.pedantic = True,
  Connect.tls_required = False,
  Connect.auth_token = Nothing,
  Connect.user = Nothing,
  Connect.pass = Nothing,
  Connect.name = Nothing,
  Connect.lang = "haskell",
  Connect.version = "0.1.0",
  Connect.protocol = Nothing,
  Connect.echo = Just True,
  Connect.sig = Nothing,
  Connect.jwt = Nothing,
  Connect.nkey = Nothing,
  Connect.no_responders = Just True,
  Connect.headers = Just True
  }

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

buildConnectPayload :: Config -> Maybe BS.ByteString -> Bool -> Connect.Connect
buildConnectPayload cfg nonce tlsRequired =
  applyAuthToConnect base (auth cfg) nonce
  where
    base = (connectConfig cfg) { Connect.tls_required = tlsRequired }

applyAuthToConnect :: Connect.Connect -> Auth -> Maybe BS.ByteString -> Connect.Connect
applyAuthToConnect base authType nonce =
  case authType of
    None ->
      base
    AuthToken token ->
      (clearAuthFields base) { Connect.auth_token = nonEmpty token }
    UserPass (user, pass) ->
      let (user', pass') = userPassFields user pass
      in (clearAuthFields base) { Connect.user = user', Connect.pass = pass' }
    NKey seed ->
      applyNKeyAuth base seed nonce
    JWT jwtInput ->
      applyJwtAuth base jwtInput nonce

clearAuthFields :: Connect.Connect -> Connect.Connect
clearAuthFields base =
  base
    { Connect.auth_token = Nothing
    , Connect.user = Nothing
    , Connect.pass = Nothing
    , Connect.sig = Nothing
    , Connect.jwt = Nothing
    , Connect.nkey = Nothing
    }

nonEmpty :: BS.ByteString -> Maybe BS.ByteString
nonEmpty value =
  if BS.null value
    then Nothing
    else Just value

userPassFields :: BS.ByteString -> BS.ByteString -> (Maybe BS.ByteString, Maybe BS.ByteString)
userPassFields user pass
  | BS.null user || BS.null pass = (Nothing, Nothing)
  | otherwise = (Just user, Just pass)

applyNKeyAuth :: Connect.Connect -> BS.ByteString -> Maybe BS.ByteString -> Connect.Connect
applyNKeyAuth base seed nonce =
  case nonce of
    Nothing -> clearAuthFields base
    Just nonceValue ->
      case NKey.signNonceWithSeed seed nonceValue of
        Left _ -> clearAuthFields base
        Right (publicKey, signature) ->
          (clearAuthFields base)
            { Connect.nkey = nonEmpty publicKey
            , Connect.sig = nonEmpty signature
            }

applyJwtAuth :: Connect.Connect -> BS.ByteString -> Maybe BS.ByteString -> Connect.Connect
applyJwtAuth base jwtInput nonce =
  case NKey.parseJwtBundle jwtInput of
    Nothing ->
      clearAuthFields base
    Just bundle ->
      let base' = (clearAuthFields base) { Connect.jwt = nonEmpty (NKey.jwtToken bundle) }
      in case nonce of
          Nothing ->
            clearAuthFields base
          Just nonceValue ->
            case NKey.signNonceWithSeed (NKey.jwtSeed bundle) nonceValue of
              Left _ -> clearAuthFields base
              Right (_, signature) ->
                base' { Connect.sig = nonEmpty signature }

pong :: Client -> IO ()
pong client = writeToClientQueue client (QueueItem Pong)

writeToClientQueue :: Client -> QueueItem -> IO ()
writeToClientQueue client item = do
  res <- case client of
    Client' {queue = q} -> enqueue q item
  case res of
    Left err -> runClient client . logMessage Error $ ("enqueueing item failed: " ++ err)
    Right () -> return ()
