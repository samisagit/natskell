{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Client (
  Client,
  MsgView (..),
  newClient,
  publish,
  subscribe,
  unsubscribe,
  ping,
  close,
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
  AuthTokenData,
  UserPassData,
  NKeyData,
  JWTTokenData,
  TLSPublicKey,
  TLSPrivateKey,
  TLSCertData,
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString           as BS
import           Data.Map                  (Map, delete, insert, lookup)
import           Lib.CallOption
import           Lib.Logger
import           MSGView
import qualified Network.Simple.TCP        as TCP
import qualified Network.Socket            as NS
import           Options
import           Parsers.Parsers
import           Pipeline.Broadcasting     as B
import qualified Pipeline.Operator         as O
import           Prelude                   hiding (lookup)
import           Sid                       (nextSID)
import           System.IO
import           System.Random             (StdGen, newStdGen)
import           Transformers.Transformers (Transformer (transform))
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

-- | Client is used to interact with the NATS server.
data Client = Client'
                { queue               :: TBQueue QueueItem
                , routes              :: TVar (Map Subject (M.Msg -> IO ()))
                , pings               :: TVar [IO ()]
                , randomGen           :: TVar StdGen
                , serverInfo          :: TMVar I.Info
                , connectionAttempts' :: TVar Int
                , poisonpill          :: TVar Bool
                , conn                :: TVar Handle
                , config              :: Config
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

  handle <- case connectOptions defaultConfig of
    []             -> error "No server pairs provided"
    (host, port):_ -> defaultConn host port

  client <- liftIO $ Client'
    <$> newTBQueueIO 1000
    <*> newTVarIO mempty
    <*> newTVarIO []
    <*> (newTVarIO =<< newStdGen)
    <*> newEmptyTMVarIO
    <*> newTVarIO 0
    <*> newTVarIO False
    <*> newTVarIO handle
    <*> pure defaultConfig

  runClient client . logAuthMethod $ auth defaultConfig
  retryLoop client

   -- ensure that the server info is initialized
  atomically $ do
    i <- takeTMVar (serverInfo client)
    putTMVar (serverInfo client) i

  return client

retryLoop :: Client -> IO ()
retryLoop client = do
    -- TODO: this should probably cycle through the servers, could base this on the retry count..
    handle <- case connectOptions (config client) of
      []             -> error "No server pairs provided"
      (host, port):_ -> defaultConn host port

    runClient client $ logDebug "Starting the client streaming threads"
    wg <- newWaitGroup 1
    runClient client $ O.run handle (poisonpill client) genericParse (router client) (queue client)
      >> (liftIO . done $ wg)

    runClient client $ logDebug "Waiting for INFO"
    atomically $ readTMVar (serverInfo client) --  TODO: or what
    runClient client $ logDebug "INFO received"

    -- attempt reconnection if the connection is lost
    wait wg
    connectAttempts <- readTVarIO (connectionAttempts' client)
    case connectAttempts of
      0 -> do
        runClient client $ logFatal "connection attempts exceeded, stopping client"
        h <- readTVarIO (conn client)
        hClose h
        exitAction . config $ client
      _ -> do
        runClient client $ logDebug "Retrying connection"
        atomically $ modifyTVar' (connectionAttempts' client) (\x -> x - 1)
        retryLoop client

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

  atomically $ writeTBQueue (queue client) (QueueItem msg)

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

  atomically $ writeTBQueue (queue client) (QueueItem unsub)

-- | ping is used to send a ping message to the NATS server.
ping :: Client -> IO () -> IO ()
ping client action = do
  runClient client $ logDebug "Sending PING to server"
  atomically $ modifyTVar' (pings client) (action :)
  atomically $ writeTBQueue (queue client) (QueueItem Ping)

-- | close is used to close the connection to the NATS server.
close :: Client -> IO ()
close client = do
  runClient client $ logInfo "Closing the client connection"
  atomically $ writeTVar (poisonpill client) True

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

  atomically $ writeTBQueue (queue client) (QueueItem sub)

  return sid

-- TODO: we could make all these monadic functions so we have access to the logger, and later the config
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
        atomically $ writeTVar (poisonpill client) True
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
  handle <- readTVarIO (conn client)
  let dat = connectConfig . config $ client
  BS.hPut handle $ transform dat
  hFlush handle

pong :: Client -> IO ()
pong client = do
  runClient client $ logDebug "Sending PONG to server"
  handle <- readTVarIO (conn client)
  BS.hPut handle $ transform Pong
  hFlush handle
