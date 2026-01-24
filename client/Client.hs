{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Client (
  Client,
  MsgView (..),
  newClient,
  publish,
  subscribe,
  unsubscribe,
  ping,
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
  LoggerConfig (..),
  LogLevel (..),
  AuthTokenData,
  UserPassData,
  NKeyData,
  JWTTokenData,
  TLSPublicKey,
  TLSPrivateKey,
  TLSCertData,
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString           as BS
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
import           MSGView
import           Network.API
import           Network.Connection
import qualified Network.Simple.TCP        as TCP
import qualified Network.Socket            as NS
import           Options
import           Parsers.Parsers
import qualified Pipeline.Broadcasting.API as B
import qualified Pipeline.Streaming.API    as S
import           Prelude                   hiding (lookup)
import           Queue.API
import           Queue.TransactionalQueue
import           Sid                       (nextSID)
import           System.IO
import           System.Random             (StdGen, newStdGen)
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
  , subscriptionExpiries  :: SubscriptionHeap
  }

emptySubscriptionState :: SubscriptionState
emptySubscriptionState = SubscriptionState mempty empty

data ConfigState = ConfigState
  { cfgConfig     :: Config
  , cfgServerInfo :: Maybe I.Info
  }

initialConfigState :: Config -> ConfigState
initialConfigState cfg = ConfigState cfg Nothing

data LifecycleState = Running | Closing | Closed

readConfigState :: Client -> IO ConfigState
readConfigState client = readTVarIO (configState client)

readConfig :: Client -> IO Config
readConfig = fmap cfgConfig . readConfigState

setServerInfo :: Client -> I.Info -> STM ()
setServerInfo client info =
  modifyTVar' (configState client) (\state -> state { cfgServerInfo = Just info })

setLifecycleClosing :: Client -> STM ()
setLifecycleClosing client =
  modifyTVar' (lifecycle client) $ \state -> case state of
    Closed  -> Closed
    _       -> Closing

markClosed :: Client -> STM Bool
markClosed client = do
  state <- readTVar (lifecycle client)
  case state of
    Closed -> return False
    _ -> writeTVar (lifecycle client) Closed >> return True

waitForClosed :: Client -> STM ()
waitForClosed client = do
  state <- readTVar (lifecycle client)
  case state of
    Closed -> return ()
    _      -> retry

waitForServerInfo :: Client -> STM ()
waitForServerInfo client = do
  cfgState <- readTVar (configState client)
  case cfgServerInfo cfgState of
    Just _  -> return ()
    Nothing -> retry

cancelExpiredSubscriptions :: Client -> IO ()
cancelExpiredSubscriptions client = do
  runClient client $ logDebug "Running subscription GC"
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
                { queue :: Q QueueItem
                , subscriptions :: TVar SubscriptionState
                , pings :: TVar [IO ()]
                , randomGen :: TVar StdGen
                , configState :: TVar ConfigState
                , connectionAttempts' :: TVar Int
                , lifecycle :: TVar LifecycleState
                , conn :: Conn
                }

-- | newClient creates a new client with optional overrides to default settings
newClient :: [(String, Int)] -> [ConfigOpts] -> IO Client
newClient servers conOpts = do
  dl <- defaultLogger
  let defaultConfig = applyCallOptions conOpts Config {
    connectConfig = defaultConnect
    , auth = None
    , loggerConfig = dl
    , connectionAttempts = 5
    , exitAction = return ()
    , connectOptions = servers
  }

  c <- Conn <$> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newEmptyTMVarIO

  client <- liftIO $ Client'
    <$> newQ
    <*> newTVarIO emptySubscriptionState
    <*> newTVarIO []
    <*> (newTVarIO =<< newStdGen)
    <*> newTVarIO (initialConfigState defaultConfig)
    <*> newTVarIO (connectionAttempts defaultConfig)
    <*> newTVarIO Running
    <*> pure c

  runClient client . logAuthMethod $ auth defaultConfig
  forkIO $ retryLoop client

  -- ensure that the server info is initialized
  atomically $ waitForServerInfo client `orElse` waitForClosed client

  return client

acquireHandle :: Client -> IO (Either String Conn)
acquireHandle client = do
  attempts <- readTVarIO (connectionAttempts' client)
  cfg <- readConfig client
  case connectOptions cfg of
    []             -> return (Left "No servers provided")
    xs -> do
      let (host, port) = cycle xs !! attempts
      h <- try @SomeException (defaultConn host port)
      case h of
        Left e -> return (Left (show e))
        Right h -> do
          point (conn client) h
          return . Right $ conn client

decomHandle ::  Either String Conn -> IO ()
decomHandle (Left _)  = return ()
decomHandle (Right h) = Network.Connection.close h

withHandle :: Client -> (Either String Conn -> IO a) -> IO a
withHandle client = bracket
    (acquireHandle client)
    decomHandle

withRetry :: Client -> Int -> IO () -> IO ()
withRetry c 0 _ = do
  runClient c $ logInfo "Ran out of retries, exiting"
  cfg <- readConfig c
  closed <- atomically $ markClosed c
  when closed $ exitAction cfg
withRetry c x action = do
  state <- readTVarIO (lifecycle c)
  case state of
    Closing -> do
      runClient c $ logInfo "Client closing, skipping retries"
      cfg <- readConfig c
      closed <- atomically $ markClosed c
      when closed $ exitAction cfg
    Closed -> return ()
    Running -> do
      action
      runClient c $ logInfo "Retrying client connection"
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
      withHandle client $ \handle -> do
        case handle of
          (Left e) -> runClient client . logError $ show e
          (Right h) -> do
            runClient client $ do
              wgs <- liftIO $ newWaitGroup 1
              wgb <- liftIO $ newWaitGroup 1
              logDebug "Starting the client streaming threads"
              liftIO . void . forkIO $ do
                runClient client $ B.run (queue client) h
                runClient client $ logDebug "Broadcasting thread has exited"
                done wgb
              liftIO . void . forkIO $ do
                runClient client $ S.run h genericParse (router client)
                runClient client $ logDebug "Streaming thread has exited"
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
                closeReader h
                done wg
              liftIO $ wait wg
              logDebug "Streaming threads have exited, closing connection"

-- | publish sends a message to the NATS server.
publish :: Client -> Subject -> [PubOptions -> PubOptions] -> IO ()
publish client subject opts = do
  runClient client . logDebug $ ("Publishing to subject: " ++ show subject)
  let (payload, callback, headers) = applyCallOptions opts (Nothing, Nothing, Nothing)

  replyTo <- case callback of
    Just cb -> do
      rand <- newHash client
      let replyTo = BS.append "RES." rand
      request client replyTo cb
      return (Just replyTo)
    Nothing -> return Nothing

  let msg = P.Pub {
    P.subject = subject,
    P.payload = payload,
    P.replyTo = replyTo,
    P.headers = headers
  }

  writeToClientQueue client (QueueItem msg)

-- | subscribe is used to subscribe to a subject on the NATS server.
subscribe :: Client -> Subject -> (Maybe MsgView -> IO ()) -> IO SID
subscribe = subscribe' False

request :: Client -> Subject -> (Maybe MsgView -> IO ()) -> IO SID
request = do
  subscribe' True

-- | unsubscribe is used to unsubscribe from a subject on the NATS server.
unsubscribe :: Client -> SID -> IO ()
unsubscribe client sid = do
  runClient client . logDebug $ ("Unsubscribing from subject with SID: " ++ show sid)
  atomically $ modifyTVar' (subscriptions client) (\s -> s { subscriptionCallbacks = delete sid (subscriptionCallbacks s) })
  let unsub = Unsub.Unsub {
    Unsub.sid = sid,
    Unsub.maxMsg = Nothing
  }

  writeToClientQueue client (QueueItem unsub)

-- | ping is used to send a ping message to the NATS server.
ping :: Client -> IO () -> IO ()
ping client action = do
  runClient client $ logDebug "Sending PING to server"
  atomically $ modifyTVar' (pings client) (action :)
  writeToClientQueue client (QueueItem Ping)

-- | close is used to close the connection to the NATS server.
close :: Client -> IO ()
close client = do
  atomically $ setLifecycleClosing client
  runClient client $ logInfo "Closing the client connection"
  Queue.API.close $ queue client
  closeReader (conn client)
  atomically $ waitForClosed client

-- internal handlers

runClient :: Client -> AppM a -> IO a
runClient client action = do
  cfg <- readConfig client
  runWithLogger (loggerConfig cfg) action

defaultConn :: String -> Int -> IO Handle
defaultConn host port = do
  (sock, _) <- TCP.connectSock host $ show port
  NS.setSocketOption sock NS.NoDelay 1
  NS.setSocketOption sock NS.Cork 0
  NS.setSocketOption sock NS.RecvBuffer 1
  NS.setSocketOption sock NS.SendBuffer 1
  handle <- NS.socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
  return handle

subscribe' :: Bool -> Client -> Subject -> (Maybe MsgView -> IO ()) -> IO SID
subscribe' isReply client subject callback = do
  runClient client . logDebug $ ("Subscribing to subject: " ++ show subject)
  sid <- newHash client
  let cb = if isReply
        then \m -> do
          callback (fmap transformMsg m)
          unsubscribe client sid
        else callback . fmap transformMsg
  -- TODO: there should probably be an option to set the expiry time
  expiry <- addUTCTime 5.0 <$> getCurrentTime
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

-- TODO: we could make all these monadic functions so we have access to the logger, and later the config
-- +1 I'd like to log these actions
router :: Client -> ParsedMessage -> IO ()
router client msg = do
  case msg of
    ParsedMsg a  -> msgRouter client a
    ParsedInfo i -> connect client >> atomically (setServerInfo client i)
    ParsedPing _ -> pong client
    ParsedPong _ -> sequenceActions (pings client)
    ParsedOk   _ -> return ()
    ParsedErr err -> case E.isFatal err of
      True  -> do
        runClient client . logError $ ("Fatal error: " ++ show err)
        Client.close client -- TODO: this should be more like a reset.. we don't want to wait for a graceful close as the server is already disconnected
      False -> runClient client . logWarn $ ("Error: " ++ show err)

logAuthMethod :: Auth -> AppM ()
logAuthMethod auth = case auth of
  None               -> logInfo "No authentication method provided"
  AuthToken _        -> logInfo "Using auth token"
  UserPass (user, _) -> logInfo $ "Using user/pass: " ++ show user
  NKey _             -> logInfo "Using NKey"
  JWT _              -> logInfo "Using JWT"
  TLSCert _          -> logInfo "Using TLS certificate"

sequenceActions :: TVar [IO ()] -> IO ()
sequenceActions actionsVar = do
  actions <- atomically $ do
    as <- readTVar actionsVar
    writeTVar actionsVar []
    return as
  sequence_ actions

msgRouter :: Client -> M.Msg -> IO ()
msgRouter client msg = do
  let sid = M.sid msg
  callbacks <- subscriptionCallbacks <$> readTVarIO (subscriptions client)
  case lookup sid callbacks of
    Just callback -> do
      runClient client . logDebug $ ("Running callback for SID: " ++ show sid)
      callback $ Just msg
    Nothing       -> runClient client . logError $ ("No callback for SID: " ++ show sid)

newHash :: Client -> IO SID
newHash client = atomically $ do
    rg <- readTVar (randomGen client)
    let (rand, stdGen) = nextSID rg
    writeTVar (randomGen client) stdGen
    return rand

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
  Connect.no_responders = Just True,
  Connect.headers = Just True
  }

connect :: Client -> IO ()
connect client = do
  runClient client $ logDebug "Connecting to the NATS server"
  cfg <- readConfig client
  let dat = connectConfig cfg
  writeToClientQueue client (QueueItem dat)

pong :: Client -> IO ()
pong client = writeToClientQueue client (QueueItem Pong)

writeToClientQueue :: Client -> QueueItem -> IO ()
writeToClientQueue client item = do
  res <- case client of
    Client' {queue = q} -> enqueue q item
  case res of
    Left err -> runClient client . logError $ ("Error enqueueing item: " ++ err)
    Right () -> return ()
