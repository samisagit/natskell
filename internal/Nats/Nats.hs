{-# LANGUAGE GADTs #-}

module Nats.Nats(
  nats,
  NatsConn(..),
  Config(..),
  NatsAPI(),
  subscriptionCallback,
  setConfig,
  addSubscription,
  removeSubscription,
  readMessage,
  sendBytes,
  recvBytes,
  socketReadLength,
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString           as BS
import qualified Data.Map                  as Map
import           Parsers.Parsers
import           Transformers.Transformers
import           Types.Msg
import           Validators.Validators

-- The NATS API

socketReadLength :: Int
socketReadLength = 1024

nats :: NatsConn a => a -> IO (NatsAPI a)
nats socket = do
  sock   <- newTMVarIO socket
  router <- newTVarIO Map.empty
  -- TODO: define a default config AND make sure places we're setting it know it's not empty
  config <- newEmptyTMVarIO
  buffer <- newTVarIO BS.empty
  return $ Nats sock router config buffer

readMessage :: NatsConn a => NatsAPI a -> IO ParsedMessage
readMessage nats = atomically $ readBuffer nats

addSubscription :: NatsConn a => NatsAPI a -> BS.ByteString -> (Msg -> IO ()) -> Maybe Int -> IO ()
addSubscription nats sid callback timeout = atomically . modifyTVar (router nats) $ Map.insert sid (callback, timeout)

removeSubscription :: NatsConn a => NatsAPI a -> BS.ByteString -> IO ()
removeSubscription nats sid = atomically . modifyTVar (router nats) $ Map.delete sid

subscriptionCallback :: NatsConn a => NatsAPI a -> BS.ByteString -> IO (Msg -> IO ())
subscriptionCallback nats sid = do
  router <- readTVarIO (router nats)
  case Map.lookup sid router of
    Nothing -> error $ "no subscription found for sid " ++ show sid
    Just (cb, _) -> return cb

recvBytes :: NatsConn a => NatsAPI a -> IO ()
recvBytes nats = do
  socket <- atomically $ takeTMVar (sock nats)
  -- since we only read here, we can safely replace the sock lock
  atomically $ putTMVar (sock nats) socket
  dat <- recv socket socketReadLength
  case dat of
    Nothing  -> do
      -- TODO: we've reached the end of input, which probably means the socket has been closed
      threadDelay 1000000
      recvBytes nats
    Just msg -> void . forkIO $ writeBuffer nats msg

sendBytes :: (Transformer m, Validator m, NatsConn a) => NatsAPI a -> m -> IO ()
sendBytes nats msg = do
  case validate msg of
    Left err -> error $ show err
    Right _  -> withSocket nats $ \socket -> send socket $ transform msg

-- TODO: this many want to be more granular if a merge isn't possible
setConfig :: NatsConn a => NatsAPI a -> Config -> IO ()
setConfig nats = atomically . putTMVar (config nats)

withSocket :: NatsConn a => NatsAPI a -> (a -> IO b) -> IO b
withSocket nats = bracket
  (atomically . takeTMVar $ sock nats)
  (atomically . putTMVar (sock nats))

-- lower level byte shuttling

class NatsConn a where
  recv :: a -> Int -> IO (Maybe BS.ByteString)
  send :: a -> BS.ByteString -> IO ()

data NatsAPI a where
  Nats :: NatsConn a => {
    sock   :: TMVar a,
    router :: TVar(Map.Map BS.ByteString (Msg -> IO (), Maybe Int)),
    config :: TMVar Config,
    buffer :: TVar BS.ByteString
  }  -> NatsAPI a

data Config = Config
  {
    maxPayload       :: Int,
    authRequired     :: Bool,
    _TLSRequired     :: Bool,
    connectURLs      :: Maybe [BS.ByteString],
    headersSupported :: Bool
  }

readBuffer :: NatsConn a => NatsAPI a -> STM ParsedMessage
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

-- TODO: this could be hidden
writeBuffer :: NatsConn a => NatsAPI a -> BS.ByteString -> IO ()
writeBuffer nats msg = atomically . modifyTVar (buffer nats) $ \b -> b <> msg

