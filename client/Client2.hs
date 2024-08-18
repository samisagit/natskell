{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Client2 (withNats, defaultConn, NatsConn, sub, pub, unsub, pubWithSubject, pubWithPayload, pubWithReplyCallback, pubWithHeaders, M.Msg(..)) where

import qualified Buffer.Buffer             as Buffer
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
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
import qualified Types.Pub                 as P
import           Types.Sub
import           Types.Unsub
import           Validators.Validators     (Validator (validate))

sub :: TVar Client -> Subject -> (M.Msg -> IO ()) -> IO SID
sub client subject callback = atomically $ sub' client subject callback

unsub :: TVar Client -> SID -> IO ()
unsub client sid = atomically $ unsub' client sid

pub :: TVar Client -> [PubOptions -> PubOptions] -> IO ()
pub client options = atomically $ pub' client options

sub' :: TVar Client -> Subject -> (M.Msg -> IO ()) -> STM SID
sub' client subject callback = do
  client' <- readTVar client
  let (sid, client'') = addRoute client' callback
  let client''' = addOutgoing client'' $ Sub subject Nothing sid
  writeTVar client client'''
  return sid

unsub' :: TVar Client -> SID -> STM ()
unsub' client sid = do
  client' <- readTVar client
  let client'' = removeRoute client' sid
  let client''' = addOutgoing client'' $ Unsub sid Nothing
  writeTVar client client'''

pub' :: TVar Client -> [PubOptions -> PubOptions] -> STM ()
pub' client options = do
  client' <- readTVar client
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

connect :: Client -> Client
connect client = do
  addOutgoing client $ Connect{
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
  let client' = client {inbox = inbox, outbox = outbox, sender = sender}
  -- consider a state enum at the top level

  -- TODO should wait for info first
  let client'' = connect client'

  tClient <- newTVarIO client''

  callback tClient -- TODO: this will need to be async so we get to loop

  loop tClient

loop :: TVar Client -> IO ()
loop c = do
  -- read incoming messages
  readMessages c
  routeMessages c
  -- send outgoing messages
  sendBytes c
  loop c

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
    router         :: Map.Map SID (M.Msg -> IO ())
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
      }
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

routeMessages :: TVar Client -> IO ()
routeMessages client = do
  client' <- readTVarIO client
  let inbox' = inbox client'
  let bs = Buffer.bytes inbox'
  case BS.length bs of
    0 -> threadDelay 1000000
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
            a -> print ("unimplemented message type: " ++ show a)

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

