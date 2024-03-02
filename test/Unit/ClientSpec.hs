{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           API
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString           as BS
import           Data.IORef
import           Harness
import qualified Nats.Nats                 as N
import           Test.Hspec
import Data.Word8
import           Control.Concurrent

type NatsHarness = (TMVar (IORef BS.ByteString, IORef [BS.ByteString]))

newNatsHarness :: IO NatsHarness
newNatsHarness = do
  msgList <- newIORef [] :: IO (IORef [BS.ByteString])
  incoming <- newIORef "" :: IO (IORef BS.ByteString)
  newTMVarIO (incoming, msgList)

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

cases = parallel $ do
  describe "Client" $ do
    it "processes a single message" $ do
      (lock, assertion) <- matcherMsg "Hello World"
      harness <- newNatsHarness
      nats <- N.nats harness
      c <- newClient nats True
      handShake c
      mockOk harness

      let subj = "SOME.EVENT"
      sid <- sub
        c
        [ subWithSubject subj,
          subWithCallback assertion
        ]
      mockOk harness
      mockPub harness $ foldl BS.append "MSG " [ subj,  " ", sid, " ", "11\r\nHello World\r\n" ]
      join . atomically $ takeTMVar lock

    it "subscribes to its reply before publishing" $ do
      harness <- newNatsHarness
      nats <- N.nats harness
      c <- newClient nats True

      handShake c
      mockOk harness

      forkIO $ mockOk harness -- OK for the sub
      forkIO $ mockOk harness -- OK for the pub
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

mockOk :: NatsHarness -> IO ()
mockOk harness = do
  (i, o) <- atomically $ takeTMVar harness
  writeIORef i "+OK\r\n"
  atomically $ putTMVar harness (i, o)
