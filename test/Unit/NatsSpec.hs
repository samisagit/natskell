{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module NatsSpec (spec) where

import           Test.Hspec
import qualified Data.ByteString           as BS
import           Control.Concurrent.STM
import           Control.Concurrent
import Data.IORef
import Nats.Nats

instance NatsConn (IORef BS.ByteString) where
  recv iobs n = do
    bs <- readIORef iobs
    let (part, rest) = BS.splitAt n bs
    writeIORef iobs rest
    return $ Just part
  send = undefined

connect ::  IO (NatsAPI (IORef BS.ByteString))
connect = do
  sock <- newIORef BS.empty
  nats sock

spec :: Spec
spec = do
  readSockCases

readSockCases :: Spec
readSockCases = do
  describe "readSock" $ do
    it "should read a message from a socket to the buffer" $ do
      nats <- connect
      let s = sock nats

      -- set the fake socket to yield a message
      msg <- newIORef "MSG foo 1 5\r\n"
      atomically $ writeTVar s msg

      -- read the message
      recvBytes nats

      -- wait for the message to be read to the buffer
      threadDelay 1

      -- check the socket has the original less 1024 bytes
      socket <- readTVarIO $ sock nats
      socketValue <- readIORef socket
      socketValue `shouldBe` ""

      -- check the buffer has the message
      bufferValue <- readTVarIO $ buffer nats
      bufferValue `shouldBe` "MSG foo 1 5\r\n"

    it "should read a partial message from a socket to the buffer" $ do
      nats <- connect
      let s = sock nats

      -- set the fake socket to have > 1024 bytes available (1040 bytes)
      let inputData = foldr BS.append BS.empty $ replicate 80 "MSG foo 1 5\r\n"
      msg <- newIORef inputData
      atomically $ writeTVar s msg

      -- read the message
      recvBytes nats

      -- wait for the message to be read to the buffer
      threadDelay 1

      -- check the socket has the original less 1024 bytes
      socket <- readTVarIO $ sock nats
      socketValue <- readIORef socket
      socketValue `shouldBe` BS.drop 1024 inputData

      -- check the buffer has the full 1024
      bufferValue <- readTVarIO $ buffer nats
      bufferValue `shouldBe` BS.take 1024 inputData

    it "should append a message from a socket to the buffer" $ do
      nats <- connect
      let s = sock nats

      -- read a message into the buffer
      msg <- newIORef "MSG foo 1 5\r\n"
      atomically $ writeTVar s msg
      recvBytes nats

      threadDelay 1

      socket <- readTVarIO $ sock nats
      socketValue <- readIORef socket
      socketValue `shouldBe` ""
      bufferValue <- readTVarIO $ buffer nats
      bufferValue `shouldBe` "MSG foo 1 5\r\n"

      -- read another message into the buffer
      msg <- newIORef "MSG foo 1 5\r\n"
      atomically $ writeTVar s msg
      recvBytes nats

      threadDelay 1

      socket <- readTVarIO $ sock nats
      socketValue <- readIORef socket
      socketValue `shouldBe` ""
      bufferValue <- readTVarIO $ buffer nats
      bufferValue `shouldBe` "MSG foo 1 5\r\nMSG foo 1 5\r\n"
