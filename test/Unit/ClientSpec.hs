{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           API
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString        as BS
import           Data.IORef
import           Data.Word8
import           Harness
import qualified Nats.Nats              as N
import           Test.Hspec

type NatsHarness = (TMVar (IORef BS.ByteString, IORef [BS.ByteString]))

newNatsHarness :: IO (Client NatsHarness, NatsHarness)
newNatsHarness = do
  msgList <- newIORef [] :: IO (IORef [BS.ByteString])
  incoming <- newIORef "" :: IO (IORef BS.ByteString)
  harness <- newTMVarIO (incoming, msgList)
  nats <- N.nats harness
  c <- newClient nats True
  forkIO $ mockOk harness -- async as we aren't reading yet
  handShake c
  return (c, harness)

instance N.NatsConn NatsHarness where
  recv sock x = do
    (i, o) <- atomically $ takeTMVar sock
    incoming <- readIORef i
    let (a, b) = BS.splitAt x incoming
    writeIORef i b
    atomically $ putTMVar sock (i, o)
    if BS.length a == 0
      then return Nothing
      else return (Just a)
  send sock x = do
    (i, o) <- atomically $ takeTMVar sock
    msgList <- readIORef o
    writeIORef o (msgList ++ [x])
    atomically $ putTMVar sock (i, o)

sentMessages :: NatsHarness -> IO [BS.ByteString]
sentMessages harness = do
  (_, o) <- atomically $ takeTMVar harness
  readIORef o

spec :: Spec
spec = do
  cases

cases :: Spec
cases =  describe "Client" $ do
    it "processes a single message" $ do
      (c, harness) <- newNatsHarness

      (lock, assertion) <- matcherMsg "Hello World"
      mockOk harness
      let subj = "SOME.EVENT"
      sid <- sub
        c
        [ subWithSubject subj,
          subWithCallback assertion
        ]
      mockPub harness $ foldl BS.append "MSG " [ subj,  " ", sid, " ", "11\r\nHello World\r\n" ]
      join . atomically $ takeTMVar lock

    it "subscribes to its reply before publishing" $ do
      (c, harness) <- newNatsHarness
      mockOk harness -- OK for the sub
      mockOk harness -- OK for the pub
      pub c [
        pubWithSubject "SOME.ENDPOINT",
        pubWithPayload "Hello World",
        pubWithReplyCallback (\_ -> return ())
        ]

      (_:sub:pub:_) <- sentMessages harness
      subjectFromSub sub `shouldBe` replyToFromPub pub

subjectFromSub :: BS.ByteString -> BS.ByteString
subjectFromSub s = head . tail $ BS.split _space s

replyToFromPub :: BS.ByteString -> BS.ByteString
replyToFromPub s = head . tail . tail $ BS.split _space s

mockPub :: NatsHarness -> BS.ByteString -> IO ()
mockPub harness msg = do
  (i, o) <- atomically $ takeTMVar harness
  readIORef i >>= writeIORef i . (msg `BS.append`)
  atomically $ putTMVar harness (i, o)
  waitForRead harness

mockOk :: NatsHarness -> IO ()
mockOk harness = do
  (i, o) <- atomically $ takeTMVar harness
  writeIORef i "+OK\r\n"
  atomically $ putTMVar harness (i, o)
  waitForRead harness

waitForRead :: NatsHarness -> IO ()
waitForRead harness = do
  (i, o) <- atomically $ takeTMVar harness
  incoming <- readIORef i
  if BS.null incoming
    then atomically $ putTMVar harness (i, o)
    else do
      atomically $ putTMVar harness (i, o)
      threadDelay 100000
      waitForRead harness

