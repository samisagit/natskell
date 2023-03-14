{-# LANGUAGE OverloadedStrings #-}

module Client(pub, sub, unsub, connect, Msg(..)) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString           as BS
import qualified Data.Map                  as Map
import qualified Network.Simple.TCP        as TCP
import           Parsers.Parsers
import           Transformers.Transformers
import           Types.Connect
import           Types.Info
import           Types.Msg
import           Types.Ping
import           Types.Pong
import           Types.Pub
import           Types.Sub
import           Validators.Validators

data Nats = Nats
  {
    sock   :: TVar TCP.Socket,
    router :: TVar(Map.Map BS.ByteString (Msg -> IO ())),
    config :: TMVar Config,
    buffer :: TVar BS.ByteString
  }

data Config = Config
  {
    maxPayload       :: Int,
    authRequired     :: Bool,
    _TLSRequired     :: Bool,
    connectURLs      :: Maybe [BS.ByteString],
    headersSupported :: Bool
  }

connect :: String -> Int -> IO Nats
connect host port = do
  (sock, _) <- TCP.connectSock host $ show port
  sockLock <- newTVarIO sock
  configLock <- newEmptyTMVarIO
  routerLock <- newTVarIO Map.empty
  bufferLock <- newTVarIO BS.empty
  let nats = Nats sockLock routerLock configLock bufferLock

  forkIO . forever $ readSock nats
  connect' nats
  forkIO . forever $ loop nats
  return nats

connect' :: Nats -> IO ()
connect' nats = do
  -- TODO: at some point we'll want to read the state set by INFO
  writeSock nats $ Connect {
    Types.Connect.verbose       = False,
    Types.Connect.pedantic      = True,
    Types.Connect.tls_required  = False,
    Types.Connect.auth_token    = Nothing,
    Types.Connect.user          = Nothing,
    Types.Connect.pass          = Nothing,
    Types.Connect.name          = Nothing,
    Types.Connect.lang          = "Haskell",
    Types.Connect.version       = "2010",
    Types.Connect.protocol      = Nothing,
    Types.Connect.echo          = Nothing,
    Types.Connect.sig           = Nothing,
    Types.Connect.jwt           = Nothing,
    Types.Connect.no_responders = Nothing,
    Types.Connect.headers       = Nothing
  }

pong :: Nats -> IO ()
pong nats = writeSock nats Pong

pub :: Nats -> BS.ByteString -> BS.ByteString -> IO ()
pub nats subject payload = writeSock nats $ Pub subject Nothing Nothing (Just payload)

sub :: Nats -> BS.ByteString -> BS.ByteString -> (Msg -> IO()) -> IO ()
sub nats sid subject callback = do
  atomically . modifyTVar (router nats) $ \m -> Map.insert sid callback m
  writeSock nats $ Sub subject Nothing sid

unsub :: Nats -> BS.ByteString -> BS.ByteString -> IO ()
unsub nats sid subject = do
  atomically . modifyTVar (router nats) $ \m -> Map.delete subject m
  writeSock nats $ Sub subject Nothing sid

readSock :: Nats -> IO ()
readSock nats = do
  socket <- readTVarIO (sock nats)
  dat    <- TCP.recv socket 1024
  case dat of
    Nothing  -> do
      -- we've reached the end of input
      threadDelay 1000000
      readSock nats
    Just msg -> void . forkIO $ writeToBuffer nats msg

writeSock :: (Transformer a, Validator a) => Nats -> a -> IO ()
writeSock nats msg = do
  case validate msg of
    Left err -> error $ show err
    Right _  -> do
      socket <- readTVarIO (sock nats)
      TCP.send socket $ transform msg

loop :: Nats -> IO ()
loop nats = do
  msg <- atomically $ readBuffer nats
  case msg of
    ParsedMsg a     -> handleMsg nats a
    ParsedPing Ping -> pong nats
    ParsedInfo a    -> setConfigFromInfo nats a
    ParsedOk _      -> return ()
    ParsedPong _    -> return ()
    -- TODO: we should check the error to see if it' fatal, if so we'll have been disconnected
    ParsedErr err   -> print $ "error: " ++ show err

readBuffer :: Nats -> STM ParsedMessage
readBuffer nats = do
  bytes <- readTVar (buffer nats)
  case genericParse bytes of
    -- it's possible that we've read a partial message, so we'll retry.
    -- TODO: it's also possible that we've read a message that we don't know how to parse
    -- so we should probably catch that somehow and enter a self repair mode, i.e. drop all
    -- bytes until we find a control keyword.
    Left _            -> retry
    Right (msg, rest) -> do
      writeTVar (buffer nats) rest
      return msg

writeToBuffer :: Nats -> BS.ByteString -> IO ()
writeToBuffer nats msg = atomically . modifyTVar (buffer nats) $ \b -> b <> msg

handleMsg :: Nats -> Msg -> IO ()
handleMsg nats msg = do
 readMap <- readTVarIO $ router nats
 case Map.lookup (Types.Msg.sid msg) readMap of
   Nothing -> print $ "no callback found for " ++ show msg
   Just f  -> f msg

setConfigFromInfo :: Nats -> Info -> IO ()
setConfigFromInfo nats info = do
  atomically . putTMVar (config nats) $ Config {
    maxPayload       = max_payload info,
    headersSupported = isTruthy (Types.Info.headers info),
    authRequired     = isTruthy (Types.Info.auth_required info),
    _TLSRequired     = isTruthy (Types.Info.tls_required info),
    connectURLs      = connect_urls info
    }

isTruthy :: Maybe Bool -> Bool
isTruthy Nothing  = False
isTruthy (Just b) = b

