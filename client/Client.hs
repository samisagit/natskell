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
  pubWithPayload,
  pubWithReplyCallback,
  pubWithHeaders,
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
import           Data.Map                  (Map, delete, insert, lookup)
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
import           Sid                       (nextSID)
import           System.IO
import           System.Random             (StdGen, newStdGen)
import           Transformers.Transformers
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

data QueueItem = forall m. Transformer m => QueueItem m

instance Transformer QueueItem where
  transform (QueueItem m) = transform m

data Q1 t = Q1 (TBQueue t) (TVar Bool)

newQ1 :: IO (Q1 QueueItem)
newQ1 = Q1 <$> newTBQueueIO 1000 <*> newTVarIO False

instance Transformer t => Queue (Q1 t) t where
  enqueue (Q1 q p) t = do
    poisoned <- readTVarIO p
    if poisoned
      then return $ Left "Queue is poisoned"
      else atomically $ do
        writeTBQueue q t
        return $ Right ()
  dequeue (Q1 q p) = do
    atomically $
      (Right <$> readTBQueue q)
      `orElse`
      (do
        poisoned <- readTVar p
        check poisoned
        return $ Left "Queue is poisoned")
  isEmpty (Q1 q p) = do
    poisoned <- readTVarIO p
    if poisoned
      then return True
      else atomically $ isEmptyTBQueue q
  close (Q1 _ p) = atomically $ writeTVar p True

-- | Client is used to interact with the NATS server.
data Client = Client'
                { queue               :: Q1 QueueItem
                , routes              :: TVar (Map Subject (M.Msg -> IO ()))
                , pings               :: TVar [IO ()]
                , randomGen           :: TVar StdGen
                , serverInfo          :: TMVar I.Info
                , connectionAttempts' :: TVar Int
                , poisonpill          :: TVar Bool
                , exited              :: TMVar ()
                , config              :: Config
                , conn                :: Conn
                }

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
    <$> newQ1
    <*> newTVarIO mempty
    <*> newTVarIO []
    <*> (newTVarIO =<< newStdGen)
    <*> newEmptyTMVarIO
    <*> newTVarIO (connectionAttempts defaultConfig)
    <*> newTVarIO False
    <*> newEmptyTMVarIO
    <*> pure defaultConfig
    <*> pure c

  runClient client . logAuthMethod $ auth defaultConfig
  forkIO $ retryLoop client

   -- ensure that the server info is initialized
  atomically $ do
    readTMVar (serverInfo client)
    return ()
    `orElse`
    readTMVar (exited client)

  return client

acquireHandle :: Client -> IO (Either String Conn)
acquireHandle client = do
  attempts <- readTVarIO (connectionAttempts' client)
  case connectOptions (config client) of
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
decomHandle (Left _) = return ()
decomHandle (Right h) = do
  Network.Connection.close h
  return ()

withHandle :: Client -> (Either String Conn -> IO a) -> IO a
withHandle client = bracket
    (acquireHandle client)
    decomHandle

withRetry :: Client -> Int -> IO () -> IO ()
withRetry c 0 _ = do
  runClient c $ logInfo "Ran out of retries, exiting"
  exitAction . config $ c
  atomically $ putTMVar (exited c) ()
  return ()
withRetry c x action = do
  pp <- readTVarIO (poisonpill c)
  when pp $ do
    runClient c $ logInfo "Client closing, skipping retries"
    exitAction . config $ c
    atomically $ putTMVar (exited c) ()
    return ()
  unless pp $ do
    action
    runClient c $ logInfo "Retrying client connection"
    atomically $ modifyTVar (connectionAttempts' c) (+1)
    withRetry c (x-1) action

retryLoop :: Client -> IO ()
retryLoop client = do
  let attemptsLeft = connectionAttempts . config $ client
  withRetry client attemptsLeft $ do
    withHandle client $ \handle -> do
      case handle of
        (Left e) -> runClient client . logError $ show e
        (Right h) -> do
          runClient client $ do
            wg <- liftIO $ newWaitGroup 2
            logDebug "Starting the client streaming threads"
            liftIO . void . forkIO $ do
              runClient client $ B.run (queue client) h
              runClient client $ logDebug "Broadcasting thread has exited"
              done wg
            liftIO . void . forkIO $ do
              runClient client $ S.run h genericParse (router client)
              runClient client $ logDebug "Streaming thread has exited"
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
subscribe :: Client -> Subject -> (MsgView -> IO ()) -> IO SID
subscribe = subscribe' False

request :: Client -> Subject -> (MsgView -> IO ()) -> IO SID
request = do
  subscribe' True

-- | unsubscribe is used to unsubscribe from a subject on the NATS server.
unsubscribe :: Client -> SID -> IO ()
unsubscribe client sid = do
  runClient client . logDebug $ ("Unsubscribing from subject with SID: " ++ show sid)
  atomically $ modifyTVar' (routes client) (delete sid)
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
  atomically $ writeTVar (poisonpill client) True
  runClient client $ logInfo "Closing the client connection"
  Queue.API.close $ queue client
  closeReader (conn client)
  atomically $ takeTMVar (exited client)

-- internal handlers

runClient :: Client -> AppM a -> IO a
runClient client action = do
  let cfg = config client
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

-- TODO: need to build in the timeout handling here
subscribe' :: Bool -> Client -> Subject -> (MsgView -> IO ()) -> IO SID
subscribe' isReply client subject callback = do
  runClient client . logDebug $ ("Subscribing to subject: " ++ show subject)
  sid <- newHash client
  let cb = if isReply
        then \m -> callback (transformMsg m) >> unsubscribe client sid
        else callback . transformMsg
  atomically $ modifyTVar' (routes client) (insert sid cb)
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
    ParsedInfo i -> connect client >> atomically (putTMVar (serverInfo client) i)
    ParsedPing _ -> pong client
    ParsedPong _ -> sequenceActions (pings client)
    ParsedOk   _ -> return ()
    ParsedErr err -> case E.isFatal err of
      True  -> do
        runClient client . logError $ ("Fatal error: " ++ show err)
        Client.close client
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
  callbacks <- readTVarIO (routes client)
  case lookup sid callbacks of
    Just callback -> do
      runClient client . logDebug $ ("Running callback for SID: " ++ show sid)
      callback msg
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
  let dat = connectConfig . config $ client
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

