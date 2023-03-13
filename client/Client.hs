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
import           Types.Msg
import           Types.Ping
import           Types.Pong
import           Types.Pub
import           Types.Sub
import           Validators.Validators


data Nats = Nats
  {
    sock   :: TCP.Socket,
    router :: TVar(Map.Map BS.ByteString (Msg -> IO ()))
  }

connect :: String -> Int -> IO Nats
connect host port = do
  (sock, _) <- TCP.connectSock host $ show port
  -- TODO: read sock here to connect using INFO context
  nats <- Nats sock <$> createRouter
  connect' nats
  return nats

connect' :: Nats -> IO ThreadId
connect' nats = do
  writeSock nats $ Connect False True False Nothing Nothing Nothing Nothing "Haskell" "2010" Nothing Nothing Nothing Nothing Nothing (Just True)
  forkIO . forever $ readSock nats

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
  dat <- TCP.recv (sock nats) 1024
  case dat of
    Nothing  -> do
      threadDelay 1000000
      readSock nats
    Just msg -> do
      void . forkIO $ handleProtoMessage nats msg

writeSock :: (Transformer a, Validator a) => Nats -> a -> IO ()
writeSock nats msg = do
  case validate msg of
    Left err -> error $ show err
    Right _  -> TCP.send (sock nats) $ transform msg

handleProtoMessage :: Nats -> BS.ByteString -> IO ()
handleProtoMessage nats msg = do
  case genericParse msg of
    Left err  -> error $ show err
    Right (msg, rest) -> do
      handleParsedMessage nats msg
      unless (BS.null rest) $ handleProtoMessage nats rest

handleParsedMessage :: Nats -> ParsedMessage -> IO ()
handleParsedMessage nats msg = do
  case msg of
    ParsedMsg a -> do
      readMap <- readTVarIO $ router nats
      case Map.lookup (Types.Msg.sid a) readMap of
        Nothing -> do
          print $ "no callback found for " ++ show msg -- TODO: decide how to handle missing subs
        Just f  -> f a
    ParsedPing Ping   -> do
      pong nats
    ParsedInfo _   -> do
      return () -- TODO: deal with further info
    _                 -> do
      print $ "unhandled message: " ++ show msg

createRouter :: IO(TVar(Map.Map BS.ByteString (Msg -> IO ())))
createRouter = newTVarIO Map.empty

