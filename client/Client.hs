{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Client (
  Client,
  newClient,
  defaultConn,
  pubWithPayload,
  pubWithReplyCallback,
  pubWithHeaders,
  M.Msg(..),
  publish,
  subscribe,
  ping,
  ) where

import           Broadcasting              as B
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad             (void)
import qualified Data.ByteString           as BS
import           Data.Map                  (Map, insert, lookup)
import           Lib.Parser                (ParserErr (..))
import qualified Network.Simple.TCP        as TCP
import qualified Network.Socket            as NS
import           Parsers.Parsers
import           Sid                       (nextSID)
import           Streaming                 as S
import           System.IO
import           Transformers.Transformers (Transformer (transform))
import           Types
import           Types.Connect             (Connect (..))
-- import qualified Types.Info                as I
import qualified Types.Msg                 as M
import           Types.Ping
import           Types.Pong
import qualified Types.Pub                 as P
import           Types.Sub
-- import           Types.Unsub
import           Prelude                   hiding (lookup)
import           System.Random             (StdGen, newStdGen)
import           WaitGroup

data Client = Client
                { queue     :: TBQueue QueueItem
                , routes    :: TVar (Map Subject (M.Msg -> IO ()))
                , pings     :: TVar [IO ()]
                , randomGen :: TVar StdGen
                }

instance SelfHealer BS.ByteString ParserErr where
  heal bs _  = (BS.tail bs, OK) -- TODO: this is a test implementation, should be replaced with a proper healing mechanism

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

newClient :: Handle -> IO Client
newClient handle = do
  pubQueue <- newTBQueueIO 1000
  routerVar <- newTVarIO mempty
  pingsVar <- newTVarIO []
  randomGenVar <- newTVarIO =<< newStdGen
  let client = Client {
    queue = pubQueue,
    routes = routerVar,
    pings = pingsVar,
    randomGen = randomGenVar
  }

  infoWaiter <- newWaitGroup 1
  void . forkIO . S.runPipeline handle genericParse $ sink client infoWaiter
  wait infoWaiter

  void . forkIO . B.runPipeline (queue client) $ handle

  return client

  where
    sink client wg msg = do
      case msg of
        ParsedMsg a  -> router client a
        ParsedInfo _ -> connect handle >> done wg
        ParsedPing _ -> pong handle
        ParsedPong _ -> sequenceActions (pings client)
        _            -> print ("unimplemented message type: " ++ show msg)

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

type PubOptions = (Maybe Payload, Maybe (M.Msg -> IO ()), Maybe Headers)

publish :: Client -> Subject -> [PubOptions -> PubOptions] -> IO ()
publish client subject opts = do
  let (payload, callback, headers) = applyPubOptions defaultPubOptions opts

  replyTo <- case callback of
    Just cb -> do
      rand <- atomically $ do
        rg <- readTVar (randomGen client)
        let (rand, stdGen) = nextSID rg
        writeTVar (randomGen client) stdGen
        return rand
      let replyTo = BS.append "RES." rand
      -- TODO: we should grab the SID and unsub when the callback completes
      subscribe client replyTo cb
      return (Just replyTo)
    Nothing -> return Nothing

  let msg = P.Pub {
    P.subject = subject,
    P.payload = payload,
    P.replyTo = replyTo,
    P.headers = headers
  }

  atomically $ writeTBQueue (queue client) (QueueItem msg)

subscribe :: Client -> Subject -> (M.Msg -> IO ()) -> IO SID
subscribe client subject callback = do
  sid <- atomically $ do
    rg <- readTVar (randomGen client)
    let (rand, stdGen) = nextSID rg
    writeTVar (randomGen client) stdGen
    return rand
  atomically $ modifyTVar' (routes client) (insert sid callback)
  let sub = Sub {
    subject = subject,
    queueGroup = Nothing,
    sid = sid
  }

  atomically $ writeTBQueue (queue client) (QueueItem sub)

  return sid

ping :: Client -> IO () -> IO ()
ping client action = do
  atomically $ modifyTVar' (pings client) (action :)
  atomically $ writeTBQueue (queue client) (QueueItem Ping)

applyPubOptions :: PubOptions -> [PubOptions -> PubOptions] -> PubOptions
applyPubOptions = foldl (flip ($))

defaultPubOptions :: PubOptions
defaultPubOptions = (Nothing, Nothing, Nothing)

pubWithPayload :: Payload -> PubOptions -> PubOptions
pubWithPayload payload (_, callback, headers) = (Just payload, callback, headers)

pubWithReplyCallback :: (M.Msg -> IO ()) -> PubOptions -> PubOptions
pubWithReplyCallback callback (payload, _, headers) = (payload, Just callback, headers)

pubWithHeaders :: Headers -> PubOptions -> PubOptions
pubWithHeaders headers (payload, callback, _) = (payload, callback, Just headers)

-- internal handlers

defaultConnect = Connect{
  verbose = False,
  pedantic = True,
  tls_required = False,
  auth_token = Nothing,
  user = Nothing,
  pass = Nothing,
  name = Nothing,
  lang = "haskell",
  version = "0.1.0",
  protocol = Nothing,
  echo = Just True,
  sig = Nothing,
  jwt = Nothing,
  no_responders = Just True,
  headers = Just True
  }

connect :: Handle -> IO ()
connect handle = do
  BS.hPut handle $ transform defaultConnect
  hFlush handle

pong :: Handle -> IO ()
pong handle = do
  BS.hPut handle $ transform Pong
  hFlush handle
