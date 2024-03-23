{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Harness where

import           API
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Data.ByteString        as BS
import           Data.IORef
import qualified Data.Text              as Text
import           Data.Text.Encoding     (encodeUtf8)
import qualified Nats.Nats              as N

type IncomingMessages = IORef BS.ByteString
type OutgoingMessages = IORef [BS.ByteString]

data NatsHarness = NatsHarness {
  incomingMessages :: MVar IncomingMessages,
  outgoingMessages :: MVar OutgoingMessages,
  asyncWait        :: MVar ()
}

newNatsHarness :: IO (Client NatsHarness, NatsHarness)
newNatsHarness = do
  asyncWait <- newEmptyMVar
  outgoing <-  newIORef [] >>= newMVar
  incoming <- newIORef "" >>= newMVar
  let harness = NatsHarness{
    incomingMessages = incoming,
    outgoingMessages = outgoing,
    asyncWait = asyncWait
    }
  nats <- N.nats harness
  c <- newClient nats [withAckCallback (putMVar asyncWait ())]
  forkIO $ mockOk harness -- async as we aren't reading yet
  handShake c
  return (c, harness)

instance N.NatsConn NatsHarness where
  recv harness x = do
    i <- takeMVar $ incomingMessages harness
    incoming <- readIORef i
    let (a, b) = BS.splitAt x incoming
    writeIORef i b
    putMVar (incomingMessages harness) i
    if BS.length a == 0
      then return Nothing
      else return (Just a)
  send harness x = do
    o <- takeMVar $ outgoingMessages harness
    msgList <- readIORef o
    writeIORef o (msgList ++ [x])
    putMVar (outgoingMessages harness) o

sentMessages :: NatsHarness -> IO [BS.ByteString]
sentMessages harness = do
  o <- takeMVar $ outgoingMessages harness
  msgs <- readIORef o
  putMVar (outgoingMessages harness) o
  return msgs

mockPub :: NatsHarness -> BS.ByteString -> IO ()
mockPub harness msg = do
  i <- takeMVar $ incomingMessages harness
  readIORef i >>= writeIORef i . (msg `BS.append`)
  putMVar (incomingMessages harness) i
  waitForAck harness

mockOk :: NatsHarness -> IO ()
mockOk harness = do
  i <- takeMVar $ incomingMessages harness
  writeIORef i "+OK\r\n"
  putMVar (incomingMessages harness) i

waitForAck :: NatsHarness -> IO ()
waitForAck harness = do
  takeMVar (asyncWait harness) -- +OK should put this MVar

chkLastMsg :: TMVar (IORef BS.ByteString, IORef [BS.ByteString]) -> (BS.ByteString -> IO()) -> IO ()
chkLastMsg socket matcher = do
  (i, o) <- atomically $ takeTMVar socket
  msgList <- readIORef o
  matcher $ last msgList
  atomically $ putTMVar socket (i, o)

packStr :: String -> BS.ByteString
packStr = encodeUtf8 . Text.pack

