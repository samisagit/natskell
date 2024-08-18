{-# LANGUAGE OverloadedStrings #-}

module BufferSpec where

import           Buffer.Buffer
import           Control.Concurrent.MVar
import           Control.Monad.Trans.State
import qualified Data.ByteString         as BS
import           Test.Hspec

spec :: Spec
spec = describe "Buffer" $ do
  it "presents buffer data for consumption" $ do
    mock <- newMock "hello"
    let puller = pull mock
    let buf = newBuffer puller
    res <- evalStateT hydrate buf
    res `shouldBe` "hello"
    getPullableData mock `shouldReturn` ""
  it "consumes data from the buffer" $ do
    mock <- newMock "hello"
    let puller = pull mock
    let buf = newBuffer puller
    res <- evalStateT (hydrate >> chomp 3) buf
    res `shouldBe` "lo"
    getPullableData mock `shouldReturn` ""
  it "consumes data exactly the buffer memory limit at a time" $ do
    let bs = BS.replicate 1025 0 -- 1 byte over the limit
    mock <- newMock bs
    let puller = pull mock
    let buf = newBuffer puller
    res <- evalStateT hydrate buf
    res `shouldBe` BS.init bs
    getPullableData mock `shouldReturn` BS.singleton 0

-- consider making these IO refs, we shouldn't count on this thread safety
-- as it isn't implemented in the Buffer module
data Mock = Mock {
  pullableData :: MVar BS.ByteString,
  pullCount    :: MVar Int
  }

newMock :: BS.ByteString -> IO Mock
newMock bs = do
  pullable <- newMVar bs
  pullCount <- newMVar 0
  return $ Mock pullable pullCount

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

getPullableData :: Mock -> IO BS.ByteString
getPullableData mock = readMVar $ pullableData mock

