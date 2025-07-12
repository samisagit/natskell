{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Client (
  Client,
  newClient,
  defaultConn,
  MsgView (..),
  publish,
  subscribe,
  unsubscribe,
  ping,
  pubWithPayload,
  pubWithReplyCallback,
  pubWithHeaders,
  withConnectName,
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad             (void)
import qualified Data.ByteString           as BS
import           Data.Map                  (Map, delete, insert, lookup)
import           Lib.Parser                (ParserErr (..))
import qualified Network.Simple.TCP        as TCP
import qualified Network.Socket            as NS
import           Parsers.Parsers
import           Pipeline.Broadcasting     as B
import           Pipeline.Streaming        as S
import           Prelude                   hiding (lookup)
import           Sid                       (nextSID)
import           System.IO
import           System.Random             (StdGen, newStdGen)
import           Transformers.Transformers (Transformer (transform))
import           Types
import qualified Types.Connect             as Connect (Connect (..))
import qualified Types.Info                as I
import qualified Types.Msg                 as M
import           Types.Ping
import           Types.Pong
import qualified Types.Pub                 as P
import qualified Types.Sub                 as Sub
import qualified Types.Unsub               as Unsub
import           WaitGroup

-- | Client is used to iteract with the NATS server.
data Client = Client'
                { queue      :: TBQueue QueueItem
                , routes     :: TVar (Map Subject (M.Msg -> IO ()))
                , pings      :: TVar [IO ()]
                , randomGen  :: TVar StdGen
                , serverInfo :: TMVar I.Info
                }

-- | MsgView represents a MSG in the NATS protocol.
data MsgView = MsgView
                 { -- | The subject of the message.
                   subject :: BS.ByteString
                   -- | The SID (subscription ID) of the message.
                 , sid     :: BS.ByteString
                   -- | The replyTo subject, if any.
                 , replyTo :: Maybe BS.ByteString
                   -- | The payload of the message, if any.
                 , payload :: Maybe BS.ByteString
                   -- | Headers associated with the message, if any.
                 , headers :: Maybe [(BS.ByteString, BS.ByteString)]
                 }
  deriving (Eq, Show)

transformMsg :: M.Msg -> MsgView
transformMsg msg = MsgView {
    subject = M.subject msg,
    sid     = M.sid msg,
    replyTo = M.replyTo msg,
    payload = M.payload msg,
    headers = M.headers msg
  }

instance SelfHealer BS.ByteString ParserErr where
  heal bs _  = (BS.tail bs, OK) -- TODO: this is a test implementation, should be replaced with a proper healing mechanism

-- | defaultConn is a sane default connection that can be used as an argument to `newClient`.
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

type ConnectOpts = Connect.Connect -> Connect.Connect

withConnectName :: BS.ByteString -> ConnectOpts
withConnectName name opts = opts { Connect.name = Just name }

-- | newClient creates a new `Client` instance and starts the necessary pipelines.
newClient :: Handle -> [ConnectOpts] -> IO Client
newClient handle conOpts = do
  client <- Client'
    <$> newTBQueueIO 1000
    <*> newTVarIO mempty
    <*> newTVarIO []
    <*> (newTVarIO =<< newStdGen)
    <*> newEmptyTMVarIO

  asyncInfo <-  newEmptyTMVarIO
  void . forkIO . S.runPipeline handle genericParse $ sink client asyncInfo
  info <- atomically $ takeTMVar asyncInfo
  atomically $ putTMVar (serverInfo client) info

  pingWaiter <- newWaitGroup 1
  ping client $ done pingWaiter
  void . forkIO . B.runPipeline (queue client) $ handle
  wait pingWaiter

  return client

  where
    connect' = foldl (flip ($)) defaultConnect conOpts
    sink client tmvar msg = do
      case msg of
        ParsedMsg a  -> router client a
        ParsedInfo i -> connect handle connect' >> atomically (putTMVar tmvar i)
        ParsedPing _ -> pong handle
        ParsedPong _ -> sequenceActions (pings client)
        ParsedErr  _ -> print ("error reported: " ++ show msg)
        ParsedOk   _ -> return ()

sequenceActions :: TVar [IO ()] -> IO ()
sequenceActions actionsVar = do
  actions <- atomically $ do
    as <- readTVar actionsVar
    writeTVar actionsVar []
    return as
  sequence_ actions

router :: Client -> M.Msg -> IO ()
router client msg = do
  let sid = M.sid msg
  callbacks <- readTVarIO (routes client)
  case lookup sid callbacks of
    Just callback -> callback msg
    Nothing       -> putStrLn $ "No callback for SID: " ++ show sid

type PubOptions = (Maybe Payload, Maybe (MsgView -> IO ()), Maybe Headers)

-- | publish sends a message to the NATS server.
publish :: Client -> Subject -> [PubOptions -> PubOptions] -> IO ()
publish client subject opts = do
  let (payload, callback, headers) = applyPubOptions defaultPubOptions opts

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

subscribe' :: Bool -> Client -> Subject -> (MsgView -> IO ()) -> IO SID
subscribe' isReply client subject callback = do
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

-- | subscribe is used to subscribe to a subject on the NATS server.
subscribe :: Client -> Subject -> (MsgView -> IO ()) -> IO SID
subscribe = subscribe' False

request :: Client -> Subject -> (MsgView -> IO ()) -> IO SID
request = subscribe' True

-- | unsubscribe is used to unsubscribe from a subject on the NATS server.
unsubscribe :: Client -> SID -> IO ()
unsubscribe client sid = do
  atomically $ modifyTVar' (routes client) (delete sid)
  let unsub = Unsub.Unsub {
    Unsub.sid = sid,
    Unsub.maxMsg = Nothing
  }

  atomically $ writeTBQueue (queue client) (QueueItem unsub)

-- | ping is used to send a ping message to the NATS server.
ping :: Client -> IO () -> IO ()
ping client action = do
  atomically $ modifyTVar' (pings client) (action :)
  atomically $ writeTBQueue (queue client) (QueueItem Ping)

applyPubOptions :: PubOptions -> [PubOptions -> PubOptions] -> PubOptions
applyPubOptions = foldl (flip ($))

defaultPubOptions :: PubOptions
defaultPubOptions = (Nothing, Nothing, Nothing)

-- | pubWithPayload is used to set the payload for a publish operation.
pubWithPayload :: Payload -> PubOptions -> PubOptions
pubWithPayload payload (_, callback, headers) = (Just payload, callback, headers)

-- | pubWithReplyCallback is used to set a callback for a reply to a publish operation.
pubWithReplyCallback :: (MsgView -> IO ()) -> PubOptions -> PubOptions
pubWithReplyCallback callback (payload, _, headers) = (payload, Just callback, headers)

-- | pubWithHeaders is used to set headers for a publish operation.
pubWithHeaders :: Headers -> PubOptions -> PubOptions
pubWithHeaders headers (payload, callback, _) = (payload, callback, Just headers)

-- internal handlers

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

connect :: Handle -> Connect.Connect -> IO ()
connect handle con = do
  BS.hPut handle $ transform con
  hFlush handle

pong :: Handle -> IO ()
pong handle = do
  BS.hPut handle $ transform Pong
  hFlush handle
