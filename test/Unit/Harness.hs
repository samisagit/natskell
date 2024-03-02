{-# LANGUAGE FlexibleInstances #-}

module Harness where

import           API
import           Control.Concurrent.STM
import qualified Data.ByteString        as BS
import           Data.IORef
import qualified Data.Text              as Text
import           Data.Text.Encoding     (encodeUtf8)
import           Test.Hspec

chkLastMsg :: TMVar (IORef BS.ByteString, IORef [BS.ByteString]) -> (BS.ByteString -> IO()) -> IO ()
chkLastMsg socket matcher = do
  (i, o) <- atomically $ takeTMVar socket
  msgList <- readIORef o
  matcher $ last msgList
  atomically $ putTMVar socket (i, o)

payloadAssertion :: BS.ByteString -> (Msg -> Expectation)
payloadAssertion matcher (Msg _ _ _ msg _) = msg `shouldBe` Just matcher

asyncAssert :: (Msg -> Expectation) -> IO (TMVar Expectation, Msg -> IO ())
asyncAssert e = do
  lock <- newEmptyTMVarIO
  let callback msg = atomically $ putTMVar lock (e msg)
  return (lock, callback)

matcherMsg :: BS.ByteString -> IO (TMVar Expectation, Msg -> IO ())
matcherMsg payload = do
  (lock, callback) <- asyncAssert (payloadAssertion payload)
  return (lock, callback)

packStr :: String -> BS.ByteString
packStr = encodeUtf8 . Text.pack

