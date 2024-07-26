{-# LANGUAGE OverloadedStrings #-}

module BufferSpec where

import           Buffer.Buffer
import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar
import qualified Data.ByteString         as BS
import           Test.Hspec

spec :: Spec
spec = do
  describe "Buffer" $ do
    it "presents buffer data for consumption" $ do
      mock <- newMock "hellohellohello"
      let puller = pull mock
      let consumer = consume mock
      withBuffer puller consumer
      threadDelay 500000
      getConsumedData mock `shouldReturn` "hellohellohello"
      getBufferData mock `shouldReturn` ""
      assertConsumeCount mock 2 -- initial call, call on data
    it "consumes further data after initial read" $ do
      mock <- newMock ""
      let puller = pull mock
      let consumer = consume mock
      withBuffer puller consumer
      threadDelay 500000
      push mock "hellohellohello"
      threadDelay 500000
      getConsumedData mock `shouldReturn` "hellohellohello"
      getBufferData mock `shouldReturn` ""
      assertConsumeCount mock 2 -- initial call, call on data
    it "consumes data exactly the buffer memory limit at a time" $ do
      let bs = BS.replicate 1024 0
      mock <- newMock bs
      let puller = pull mock
      let consumer = consume mock
      withBuffer puller consumer
      threadDelay 500000
      getConsumedData mock `shouldReturn` bs
      getBufferData mock `shouldReturn` ""
      assertConsumeCount mock 2 -- initial call, call on data, call on data
    it "consumes data marginally greater than the buffer memory limit" $ do
      let bs = BS.replicate 1025 0
      mock <- newMock bs
      let puller = pull mock
      let consumer = consume mock
      withBuffer puller consumer
      threadDelay 500000
      getConsumedData mock `shouldReturn` bs
      getBufferData mock `shouldReturn` ""
      assertConsumeCount mock 3 -- initial call, call on data, call on data
    it "backs off pulling if no data is pullable" $ do
      mock <- newMock ""
      let puller = pull mock
      let consumer = consume mock
      withBuffer puller consumer
      threadDelay 10000000
      count <- readMVar $ pullCount mock
      count < 20 `shouldBe` True -- this is a bit shit, but it suggests it's not pinning the thread

data Mock = Mock {
  pullableData :: MVar BS.ByteString,
  consumedData :: MVar BS.ByteString,
  pullCount    :: MVar Int,
  consumeCount :: MVar Int
  }

newMock :: BS.ByteString -> IO Mock
newMock bs = do
  pullable <- newMVar bs
  consumed <- newMVar ""
  pullCount <- newMVar 0
  consumeCount <- newMVar 0
  return $ Mock pullable consumed pullCount consumeCount

consume :: Mock -> BS.ByteString -> IO Int
consume mock x = do
  consumed <- takeMVar $ consumedData mock
  putMVar (consumedData mock) (consumed <> x)
  modifyMVar_ (consumeCount mock) $ \x -> return $ x + 1
  return $ BS.length x

assertConsumeCount :: Mock -> Int -> IO ()
assertConsumeCount mock x = do
  count <- readMVar $ consumeCount mock
  count `shouldBe` x

pull :: Mock -> Int -> IO (Maybe BS.ByteString)
pull mock x = do
  pullable <- takeMVar $ pullableData mock
  let (pulled, rest) = BS.splitAt x pullable
  putMVar (pullableData mock) rest
  modifyMVar_ (pullCount mock) $ \x -> return $ x + 1
  return $ Just pulled

push :: Mock -> BS.ByteString -> IO ()
push mock bs = do
  pullable <- takeMVar $ pullableData mock
  putMVar (pullableData mock) (pullable <> bs)

getBufferData :: Mock -> IO BS.ByteString
getBufferData mock = readMVar $ pullableData mock

getConsumedData :: Mock -> IO BS.ByteString
getConsumedData mock = readMVar $ consumedData mock

