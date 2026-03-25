module Client.Internal
  ( callbacksApi
  , connectionApi
  , lifecycleApi
  , nuidApi
  , publishInternal
  , pingInternal
  , flushInternal
  , resetClient
  , closeClient
  , retryLoop
  , runtimeApi
  , sidApi
  ) where

import           Client.Auth               (buildConnectPayload)
import           Client.Callbacks
    ( awaitCallbackDrain
    , enqueueCallback
    , startCallbackWorkers
    )
import           Client.CallbacksAPI       (CallbacksAPI (..))
import           Client.Lifecycle
    ( markClosed
    , setLifecycleClosing
    , setServerInfo
    , updateLogContextFromInfo
    , waitForClosed
    , waitForClosing
    , waitForServerInfo
    )
import           Client.LifecycleAPI
    ( ClientExitReason (..)
    , LifecycleAPI (..)
    , LifecycleState (..)
    )
import           Client.PublishAPI         (PublishConfig)
import           Client.Runtime
    ( readConfig
    , readConfigState
    , runClient
    , writeToClientQueue
    )
import           Client.RuntimeAPI
    ( ClientState (..)
    , Config (..)
    , ConfigState (..)
    , RuntimeAPI (..)
    )
import           Client.Subscription       (subscriptionApi)
import           Client.SubscriptionAPI
    ( SubscribeConfig (..)
    , SubscriptionAPI (..)
    )
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString           as BS
import           Data.Maybe
import           Lib.Logger
import qualified Lib.Parser                as Parser
import           Network.ConnectionAPI
    ( Conn
    , ConnectionAPI (..)
    , ReaderAPI (..)
    , TransportOption (..)
    , connectionApi
    )
import           NuidAPI                   (nuidApi)
import           Parsers.Parsers
import           Pipeline.Broadcasting     (broadcastingApi)
import           Pipeline.Broadcasting.API (BroadcastingAPI (..))
import           Pipeline.Streaming        (streamingApi)
import           Pipeline.Streaming.API    (StreamingAPI (..))
import qualified Queue.API                 as Queue
import           Queue.API                 (QueueItem (..))
import           SidAPI                    (sidApi)
import qualified Types.Connect             as Connect (Connect (..))
import qualified Types.Err                 as E
import qualified Types.Info                as I
import           Types.Msg                 (Subject)
import           Types.Ping
import           Types.Pong
import qualified Types.Pub                 as P
import           WaitGroup

runtimeApi :: RuntimeAPI
runtimeApi = RuntimeAPI
  { runtimeReadConfigState = readConfigState
  , runtimeReadConfig = readConfig
  , runtimeRunClient = runClient loggerApi
  , runtimeWriteToClientQueue = writeToClientQueue loggerApi
  }

lifecycleApi :: LifecycleAPI ClientState
lifecycleApi = LifecycleAPI
  { lifecycleUpdateLogContextFromInfo = updateLogContextFromInfo loggerApi
  , lifecycleSetServerInfo = setServerInfo
  , lifecycleSetClosing = setLifecycleClosing
  , lifecycleMarkClosed = markClosed
  , lifecycleWaitForClosed = waitForClosed
  , lifecycleWaitForServerInfo = waitForServerInfo
  , lifecycleWaitForClosing = waitForClosing
  }

callbacksApi :: CallbacksAPI
callbacksApi = CallbacksAPI
  { callbacksEnqueue = enqueueCallback
  , callbacksStartWorkers = startCallbackWorkers runtimeApi lifecycleApi
  , callbacksAwaitDrain = awaitCallbackDrain lifecycleApi
  }

publishInternal :: ClientState -> Subject -> PublishConfig -> IO ()
publishInternal client subject (payload, callback, headers) = do
  runtimeRunClient runtimeApi client . logMessage Debug $
    ("publishing to subject: " ++ show subject)

  case callback of
    Just cb -> do
      inbox <- subscriptionNextInbox subscriptionApi nuidApi client
      subscriptionSubscribe subscriptionApi runtimeApi sidApi True client inbox (SubscribeConfig Nothing) cb
      let msg = P.Pub
            { P.subject = subject
            , P.payload = payload
            , P.replyTo = Just inbox
            , P.headers = headers
            }
      runtimeWriteToClientQueue runtimeApi client (QueueItem msg)
    Nothing -> do
      let msg = P.Pub
            { P.subject = subject
            , P.payload = payload
            , P.replyTo = Nothing
            , P.headers = headers
            }
      runtimeWriteToClientQueue runtimeApi client (QueueItem msg)

pingInternal :: ClientState -> IO () -> IO ()
pingInternal client action = do
  runtimeRunClient runtimeApi client $ logMessage Debug "sending ping to server"
  atomically $ writeTQueue (pings client) action
  runtimeWriteToClientQueue runtimeApi client (QueueItem Ping)

flushInternal :: ClientState -> IO ()
flushInternal client = do
  ponged <- newEmptyTMVarIO
  pingInternal client (atomically (void (tryPutTMVar ponged ())))
  atomically $
    readTMVar ponged `orElse` lifecycleWaitForClosing lifecycleApi client

shutdownClient :: ClientState -> ClientExitReason -> String -> IO ()
shutdownClient client reason message = do
  atomically $ lifecycleSetClosing lifecycleApi client reason
  runtimeRunClient runtimeApi client $ logMessage Info message
  Queue.queueClose (queue client)
  readerClose (connectionReaderApi connectionApi) (conn client)
  atomically $ lifecycleWaitForClosed lifecycleApi client
  callbacksAwaitDrain callbacksApi client

closeClient :: ClientState -> IO ()
closeClient client = shutdownClient client ExitClosedByUser "closing client connection"

resetClient :: ClientState -> IO ()
resetClient client = shutdownClient client ExitResetRequested "resetting client connection"

acquireTransport :: ClientState -> IO (Either String (Conn, (String, Int)))
acquireTransport client = do
  attempts <- readTVarIO (connectionAttempts' client)
  cfg <- runtimeReadConfig runtimeApi client
  case connectOptions cfg of
    []             -> return (Left "No servers provided")
    xs -> do
      let (host, port) = cycle xs !! attempts
      updateLogContext (logContext client) (\ctx -> ctx { lcServer = Just (host ++ ":" ++ show port) })
      connectResult <- connectionConnectTcp connectionApi (conn client) host port
      case connectResult of
        Left err -> return (Left err)
        Right () -> return . Right $ (conn client, (host, port))

decomTransport ::  Either String (Conn, (String, Int)) -> IO ()
decomTransport (Left _)       = return ()
decomTransport (Right (h, _)) = connectionClose connectionApi h

withTransport :: ClientState -> (Either String (Conn, (String, Int)) -> IO a) -> IO a
withTransport client = bracket
    (acquireTransport client)
    decomTransport

withRetry :: ClientState -> Int -> IO () -> IO ()
withRetry c 0 _ = do
  runtimeRunClient runtimeApi c $ logMessage Info "retries exhausted; exiting"
  cfg <- runtimeReadConfig runtimeApi c
  result <- atomically $ lifecycleMarkClosed lifecycleApi c (ExitRetriesExhausted Nothing)
  maybe (return ()) (exitAction cfg) result
withRetry c x action = do
  state <- readTVarIO (lifecycle c)
  case state of
    Closing _ -> do
      runtimeRunClient runtimeApi c $ logMessage Info "client closing; skipping retries"
      cfg <- runtimeReadConfig runtimeApi c
      result <- atomically $ lifecycleMarkClosed lifecycleApi c ExitClosedByUser
      maybe (return ()) (exitAction cfg) result
    Closed _ -> return ()
    Running -> do
      action
      when (x > 1) $ do
        runtimeRunClient runtimeApi c $ logMessage Info "retrying client connection"
        atomically $ modifyTVar (connectionAttempts' c) (+1)
      withRetry c (x-1) action

withSubscriptionGC :: ClientState -> IO () -> IO ()
withSubscriptionGC client action = bracket
  (forkIO . forever . subscriptionCancelExpired subscriptionApi runtimeApi callbacksApi $ client)
  (\tid -> do
    subscriptionAwaitGC subscriptionApi client
    killThread tid
  )
  (const action)

retryLoop :: ClientState -> IO ()
retryLoop client = do
  cfg <- runtimeReadConfig runtimeApi client
  let attemptsLeft = connectionAttempts cfg
      maxBuffer = bufferLimit cfg
  withRetry client attemptsLeft $ do
    Queue.queueOpen (queue client)
    connectionOpen connectionApi (conn client)
    withSubscriptionGC client $ do
      withTransport client $ \transportResult -> do
        case transportResult of
          (Left e) -> runtimeRunClient runtimeApi client . logMessage Error $
            ("connection attempt failed: " ++ show e)
          (Right (connection, (host, _port))) -> do
            initResult <- initializeConnection client connection host
            case initResult of
              Left err ->
                runtimeRunClient runtimeApi client . logMessage Error $
                  ("connection initialization failed: " ++ err)
              Right () -> do
                resubscribeIfNeeded client
                runtimeRunClient runtimeApi client $ do
                  wgs <- liftIO $ newWaitGroup 1
                  wgb <- liftIO $ newWaitGroup 1
                  logMessage Debug "starting client streaming threads"
                  liftIO . void . forkWaitGroup wgb $ do
                    runtimeRunClient runtimeApi client $
                      broadcastingRun broadcastingApi maxBuffer (queue client) (connectionWriterApi connectionApi) connection
                    runtimeRunClient runtimeApi client $ logMessage Debug "broadcasting thread exited"
                  liftIO . void . forkWaitGroup wgs $ do
                    runtimeRunClient runtimeApi client $
                      streamingRun streamingApi maxBuffer (connectionReaderApi connectionApi) connection genericParse (router client)
                    runtimeRunClient runtimeApi client $ logMessage Debug "streaming thread exited"
                  wg <- liftIO $ newWaitGroup 2
                  liftIO . void . forkWaitGroup wg $ do
                    wait wgs
                    -- close broadcasting
                    liftIO $ Queue.queueClose (queue client)
                  liftIO . void . forkWaitGroup wg $ do
                    wait wgb
                    -- close streaming
                    readerClose (connectionReaderApi connectionApi) connection
                  liftIO $ wait wg
                  logMessage Debug "streaming threads exited; closing connection"

resubscribeIfNeeded :: ClientState -> IO ()
resubscribeIfNeeded client = do
  shouldResubscribe <- atomically $ do
    alreadyConnected <- readTVar (connectedOnce client)
    writeTVar (connectedOnce client) True
    return alreadyConnected
  when shouldResubscribe (subscriptionResubscribeAll subscriptionApi runtimeApi client)

initializeConnection :: ClientState -> Conn -> String -> IO (Either String ())
initializeConnection client h host = do
  infoResult <- readInitialInfo client h
  case infoResult of
    Left err -> return (Left err)
    Right (info, rest) -> do
      cfg <- runtimeReadConfig runtimeApi client
      let tlsRequested = isJust (tlsCert cfg) || Connect.tls_required (connectConfig cfg)
          tlsRequired = fromMaybe False (I.tls_required info)
          transportOption = TransportOption
            { transportHost = host
            , transportTlsRequired = tlsRequired
            , transportTlsRequested = tlsRequested
            , transportTlsCert = tlsCert cfg
            , transportInitialBytes = rest
            }
      transportResult <- connectionConfigure connectionApi h transportOption
      case transportResult of
        Left transportErr -> return (Left transportErr)
        Right () -> do
          runtimeRunClient runtimeApi client $ routerM client (ParsedInfo info)
          return (Right ())

readInitialInfo :: ClientState -> Conn -> IO (Either String (I.Info, BS.ByteString))
readInitialInfo _client h =
  go mempty
  where
    go acc = do
      result <- readerReadData (connectionReaderApi connectionApi) h 4096
      case result of
        Left err -> return (Left err)
        Right chunk ->
          if BS.null chunk
            then return (Left "read returned empty chunk before INFO")
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

router :: ClientState -> ParsedMessage -> IO ()
router client = runtimeRunClient runtimeApi client . routerM client

routerM :: ClientState -> ParsedMessage -> AppM ()
routerM client msg = do
  case msg of
    ParsedMsg a  -> do
      logMessage Debug $ "routing MSG: " ++ show a
      subscriptionMsgRouterM subscriptionApi callbacksApi client a
    ParsedInfo i -> do
      logMessage Debug $ "routing INFO: " ++ show i
      liftIO $ do
        atomically (lifecycleSetServerInfo lifecycleApi client i)
        lifecycleUpdateLogContextFromInfo lifecycleApi client i
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
            atomically $ lifecycleSetClosing lifecycleApi client (ExitServerError err)
            void (forkIO (resetClient client))
        False -> logMessage Warn $ "server error: " ++ show err

runPingAction :: TQueue (IO ()) -> IO ()
runPingAction actionsQueue = do
  action <- atomically $ tryReadTQueue actionsQueue
  fromMaybe (return ()) action

connect :: ClientState -> IO ()
connect client = do
  runtimeRunClient runtimeApi client $ logMessage Debug "connecting to NATS server"
  cfg <- runtimeReadConfig runtimeApi client
  info <- cfgServerInfo <$> runtimeReadConfigState runtimeApi client
  let nonce = info >>= I.nonce
  let infoTlsRequired = fromMaybe False (info >>= I.tls_required)
      tlsActive = infoTlsRequired || isJust (tlsCert cfg) || Connect.tls_required (connectConfig cfg)
  let dat = buildConnectPayload cfg nonce tlsActive
  runtimeWriteToClientQueue runtimeApi client (QueueItem dat)

pong :: ClientState -> IO ()
pong client = runtimeWriteToClientQueue runtimeApi client (QueueItem Pong)
