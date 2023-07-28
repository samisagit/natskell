{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Harness where

import           Client
import           Control.Concurrent.STM
import qualified Data.ByteString           as BS
import           Data.IORef
import qualified Data.Text                 as Text
import           Data.Text.Encoding        (encodeUtf8)
import           Test.Hspec
import           Transformers.Transformers

chkLastMsg :: (Transformer m) => TMVar (BS.ByteString, IORef [BS.ByteString]) -> m -> IO ()
chkLastMsg socket msg = do
  (i, o) <- atomically $ takeTMVar socket
  msgList <- readIORef o
  last msgList `shouldBe` transform msg
  atomically $ putTMVar socket (i, o)

matcherExp :: BS.ByteString -> BS.ByteString
matcherExp matcher = BS.concat ["MSG ", matcher, " ", matcher, " ", (packStr . show) byteLength, "\r\n", matcher, "\r\n"]
  where
    byteLength = BS.length matcher

matcherAssertion :: BS.ByteString -> (Msg -> Expectation)
matcherAssertion matcher msg = msg `shouldBe` Msg matcher matcher Nothing (Just matcher) Nothing

asyncAssert :: (Msg -> Expectation) -> IO (TMVar Expectation, Msg -> IO ())
asyncAssert e = do
  lock <- newEmptyTMVarIO
  let callback msg = atomically $ putTMVar lock (e msg)
  return (lock, callback)

packStr :: String -> BS.ByteString
packStr = encodeUtf8 . Text.pack

matcherMsg :: BS.ByteString -> IO (TMVar Expectation, Msg -> IO (), BS.ByteString)
matcherMsg matcher = do
  let msg = matcherExp matcher
  (lock, callback) <- asyncAssert (matcherAssertion matcher)
  return (lock, callback, msg)
