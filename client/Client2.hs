{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Client2 (withNats, defaultConn, NatsConn, stop, sub, pub, unsub, ping, pubWithSubject, pubWithPayload, pubWithReplyCallback, pubWithHeaders, M.Msg(..)) where

import qualified Buffer.Buffer             as Buffer
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.ByteString           as BS
import qualified Data.Map                  as Map
import qualified Network.Simple.TCP        as TCP
import           Parsers.Parsers
import           Sid                       (nextSID)
import           System.Random
import           Transformers.Transformers (Transformer (transform))
import           Types
import           Types.Connect             (Connect (..))
import qualified Types.Msg                 as M
import           Types.Ping
import           Types.Pong
import qualified Types.Pub                 as P
import           Types.Sub
import           Types.Unsub
import           qualified Types.Info as I
import           Validators.Validators     (Validator (validate))

data Status = DEFAULT | CLOSING

connect :: TVar Client -> IO ()
connect = atomically . connect'

sub :: TVar Client -> Subject -> (M.Msg -> IO ()) -> IO SID
sub client subject callback = atomically $ sub' client subject callback

unsub :: TVar Client -> SID -> IO ()
unsub client sid = atomically $ unsub' client sid

pub :: TVar Client -> [PubOptions -> PubOptions] -> IO ()
pub client options = atomically $ pub' client options

pong :: TVar Client -> IO ()
pong = atomically . pong'

ping :: TVar Client -> IO ()
ping = atomically . ping'

stop :: TVar Client -> IO ()
stop = atomically . stop'

stop' :: TVar Client -> STM ()
stop' client = do
  client' <- readTVar client
  let client'' = client' {status = CLOSING}
  writeTVar client client''

ping' :: TVar Client -> STM ()
ping' client = do
  client' <- readTVar client
  case status client' of
    CLOSING -> return () -- TODO: we should inform the user
    _ -> do
      let client'' = addOutgoing client' Ping
      writeTVar client client''

pong' :: TVar Client -> STM ()
pong' client = do
  client' <- readTVar client
  case status client' of
    CLOSING -> return () -- TODO: we should inform the user
    _ -> do
      let client'' = addOutgoing client' $ Pong{}
      writeTVar client client''

connect' :: TVar Client -> STM ()
connect' client = do
  client' <- readTVar client
  case status client' of
    CLOSING -> return () -- TODO: we should inform the user
    _ -> do
      let client'' = addOutgoing client' $ Connect{
        verbose = False,
        pedantic = False,
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
        no_responders = Nothing,
        headers = Nothing
        }
      writeTVar client client''

sub' :: TVar Client -> Subject -> (M.Msg -> IO ()) -> STM SID
sub' client subject callback = do
  client' <- readTVar client
  case status client' of
    CLOSING -> return "" -- TODO: we should inform the user
    _ -> do
      let (sid, client'') = addRoute client' callback
      let client''' = addOutgoing client'' $ Sub subject Nothing sid
      writeTVar client client'''
      return sid

unsub' :: TVar Client -> SID -> STM ()
unsub' client sid = do
  client' <- readTVar client
  case status client' of
    CLOSING -> return () -- TODO: we should inform the user
    _ -> do
      let client'' = removeRoute client' sid
      let client''' = addOutgoing client'' $ Unsub sid Nothing
      writeTVar client client'''

pub' :: TVar Client -> [PubOptions -> PubOptions] -> STM ()
pub' client options = do
  client' <- readTVar client
  case status client' of
    CLOSING -> return () -- TODO: we should inform the user
    _ -> do
      let (subject, payload, callback, headers) = applyPubOptions defaultPubOptions options
      case callback of
        Nothing -> do
          let client'' = addOutgoing client' $ P.Pub subject Nothing headers (Just payload)
          writeTVar client client''
        Just cb -> do
          sid <- sub' client subject cb
          let replyTo = foldr BS.append "" ["INBOX.", subject, ".", sid]
          let client'' = addOutgoing client' $ P.Pub subject (Just replyTo) Nothing (Just payload)
          writeTVar client client''

type PubOptions = (Subject, Payload, Maybe (M.Msg -> IO ()), Maybe Headers)

applyPubOptions :: PubOptions -> [PubOptions -> PubOptions] -> PubOptions
applyPubOptions = foldl (flip ($))

defaultPubOptions :: PubOptions
defaultPubOptions = ("", "", Nothing, Nothing)

pubWithSubject :: Subject -> PubOptions -> PubOptions
pubWithSubject subject (_, payload, callback, headers) = (subject, payload, callback, headers)

pubWithPayload :: Payload -> PubOptions -> PubOptions
pubWithPayload payload (subject, _, callback, headers) = (subject, payload, callback, headers)

pubWithReplyCallback :: (M.Msg -> IO ()) -> PubOptions -> PubOptions
pubWithReplyCallback callback (subject, payload, _, headers) = (subject, payload, Just callback, headers)

pubWithHeaders :: Headers -> PubOptions -> PubOptions
pubWithHeaders headers (subject, payload, callback, _) = (subject, payload, callback, Just headers)

defaultConn :: String -> Int -> IO TCP.Socket
defaultConn host port = do
  (sock, _) <- TCP.connectSock host $ show port
  return sock

withNats :: NatsConn c => [ClientOption] -> c -> (TVar Client -> IO x) -> IO ()
withNats opts conn callback = do
  -- apply initial opts and set puller
  let client = newClient opts

  let inbox = Buffer.newBuffer (recv conn)
  let outbox =  []
  let sender = send conn
  let client' = client {inbox = inbox, outbox = outbox, sender = sender, closer = close conn}
  tClient <- newTVarIO client'

  waitForInfo tClient
  routeMessages tClient

  connect tClient
  sendBytes tClient

  callback tClient -- TODO: this will need to be async so we get to loop, but we need to know when it's done... or we make loop async, but keep track of it to cancel on close.. that would be nicer
  sendBytes tClient

  loop tClient

loop :: TVar Client -> IO ()
loop c = do
  a <- newEmptyTMVarIO
  b <- newEmptyTMVarIO
  -- read incoming messages
  void $ forkIO (readMessages c >> routeMessages c >> atomically (putTMVar a ()))
  -- send outgoing messages
  void $ forkIO (sendBytes c >> atomically (putTMVar b ()))
  --wait for them to resolve
  atomically $ takeTMVar b
  atomically $ takeTMVar a
  --decide what to do next
  client <- readTVarIO c
  case status client of
    CLOSING -> cleanup c
    _       -> loop c

cleanup :: TVar Client -> IO ()
cleanup client = do
  client' <- readTVarIO client
  -- if we've got nothing left to send then close the conn, otherwise send first then close
  case length . outbox $ client' of
    0 -> closer client'
    _ -> sendBytes client >> cleanup client

-- internal workings

class NatsConn a where
  recv :: a -> Int -> IO (Maybe BS.ByteString)
  send :: a -> BS.ByteString -> IO ()
  close :: a -> IO ()

instance NatsConn TCP.Socket where
  recv = TCP.recv
  send = TCP.send
  close = TCP.closeSock

type ClientOption = Client -> Client

data ConnectionOptions = ConnectionOptions
  {
    host       :: String,
    port       :: Int,
    okAck      :: Maybe (IO ()),
    maxPayload :: Int
  }

data Client = Client
  {
    stdGen         :: StdGen,
    inbox          :: Buffer.Buffer,
    outbox         :: [BS.ByteString],
    sender         :: BS.ByteString -> IO (),
    connectionOpts :: ConnectionOptions,
    router         :: Map.Map SID (M.Msg -> IO ()),
    status         :: Status,
    closer         :: IO (),
    clientID       :: Maybe Int
  }

initialClient :: Client
initialClient = Client {
    stdGen = mkStdGen 0,
    inbox = undefined,
    outbox = undefined,
    sender = undefined,
    router = Map.empty,
    connectionOpts = ConnectionOptions
      {
        host = "localhost",
        port = 4222,
        okAck = Nothing,
        maxPayload = 1024
      },
    status = DEFAULT,
    closer = undefined,
    clientID = Nothing
  }

newClient :: [ClientOption] -> Client
newClient co = do
  apply co initialClient

apply :: [a -> a] -> a -> a
apply (x:xs) a = x (apply xs a)
apply [] a     = a

readMessages :: TVar Client -> IO ()
readMessages client = do
  client' <- readTVarIO client
  inbox' <- execStateT Buffer.hydrate (inbox client')
  atomically . writeTVar client $ client' {inbox = inbox'}

waitForInfo :: TVar Client -> IO ()
waitForInfo client = do
  readMessages client
  client' <- readTVarIO client
  let bytes = Buffer.bytes . inbox $ client'
  case BS.length bytes of
    0 -> threadDelay 10000 >> waitForInfo client
    _ -> return ()

routeMessages :: TVar Client -> IO ()
routeMessages client = do
  client' <- readTVarIO client
  let inbox' = inbox client'
  let bs = Buffer.bytes inbox'
  case BS.length bs of
    0 -> threadDelay 10000
    _ -> do
      case genericParse bs of
        -- TODO: alter some top level state to indicate a parse error
        Left err -> error (show err)
        Right (msg, rest) -> do
          -- remove the parsed message from the buffer
          inbox'' <- execStateT (Buffer.chomp (BS.length bs - BS.length rest)) inbox'
          atomically . writeTVar client $ client' {inbox = inbox''}

          -- route the message
          case msg of
            ParsedMsg a     -> do
              case Map.lookup (M.sid a) (router client') of
                Just f' -> f' a
                Nothing -> do
                  let r = show (Map.keys . router $ client')
                  print ("missing route for SID " ++ (show . M.sid $ a) ++ " in map " ++ r)
            ParsedInfo i -> setClientID (I.client_id i) client
            ParsedPing _ -> do
              pong client >> sendBytes client
            a -> print ("unimplemented message type: " ++ show a)

setClientID :: Maybe Int -> TVar Client -> IO ()
setClientID cid client = do
  client' <- readTVarIO client
  atomically . writeTVar client $ client' {clientID = cid}

sendBytes :: TVar Client -> IO ()
sendBytes client = do
  msgs <- atomically . emptyOutbox $ client
  client' <- readTVarIO client
  case msgs of
    [] -> return ()
    msgs  -> do
        let handler = (\e -> print (show e) >> (atomically . replaceOutbox client $ msgs)) :: SomeException -> IO ()
        catch (sender client' $ BS.concat msgs) handler

emptyOutbox :: TVar Client -> STM [BS.ByteString]
emptyOutbox client = do
  client' <- readTVar client
  writeTVar client client' {outbox = []}
  return $ outbox client'

replaceOutbox :: TVar Client -> [BS.ByteString] -> STM ()
replaceOutbox client msgs = do
  client' <- readTVar client
  writeTVar client client' {outbox = msgs ++ outbox client'}

addOutgoing :: Transformer a => Validator a => Client -> a -> Client
addOutgoing client msg = case validate msg of
    Left err -> error $ show err
    Right _  -> do
      let outbox' = outbox client
      client { outbox = outbox' ++ [ transform msg] }

addRoute :: Client -> (M.Msg -> IO ()) -> (SID, Client)
addRoute client callback = do
  let (sid, gen) = nextSID $ stdGen client
  let router' = router client
  (sid, client { router = Map.insert sid callback router', stdGen = gen})

removeRoute :: Client -> SID -> Client
removeRoute client sid = do
  let router' = router client
  client { router = Map.delete sid router' }

