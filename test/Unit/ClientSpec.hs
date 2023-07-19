{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           Client
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString           as BS
import           Data.IORef
import qualified Data.Text                 as Text
import           Data.Text.Encoding        (encodeUtf8)
import qualified Nats.Nats                 as N
import           Test.Hspec
import           Transformers.Transformers
import           Types.Sub

-- needs to take an IO ref of a [ByteString] in order to read it elsewhere
instance N.NatsConn (TMVar (BS.ByteString, IORef [BS.ByteString])) where
  recv sock x = do
    (i, o) <- atomically $ takeTMVar sock
    let (a, b) = BS.splitAt x i
    atomically $ putTMVar sock (b, o)
    if BS.length a == 0
      then return Nothing
      else return (Just a)
  send sock x = do
    (i, o) <- atomically $ takeTMVar sock
    msgList <- readIORef o
    writeIORef o (msgList ++ [x])
    atomically $ putTMVar sock (i, o)

spec :: Spec
spec = do
  cases

cases = parallel $ do
  describe "Client" $ do
    it "processes a single message" $ do
      let matcher = "1" -- arbitrary bytestring to match on
      let msg = matcherMsg matcher
      (lock, assertion) <- asyncAssert (matcherAssertion matcher)
      msgList <- newIORef [] :: IO (IORef [BS.ByteString])
      -- dump the msg onto the 'socket'
      socket <- newTMVarIO (msg, msgList)
      nats <- N.nats socket
      sub
        nats
        [ subWithSubject matcher,
          subWithSID matcher,
          subWithCallback assertion
        ]
      chkLastMsg socket $ Sub "1" Nothing "1"

      handShake nats
      join . atomically $ takeTMVar lock

chkLastMsg :: (Transformer m) => TMVar (BS.ByteString, IORef [BS.ByteString]) -> m -> IO ()
chkLastMsg socket msg = do
  (i, o) <- atomically $ takeTMVar socket
  msgList <- readIORef o
  last msgList `shouldBe` transform msg
  atomically $ putTMVar socket (i, o)

matcherMsg :: BS.ByteString -> BS.ByteString
matcherMsg matcher = BS.concat ["MSG ", matcher, " ", matcher, " ", (packStr . show) byteLength, "\r\n", matcher, "\r\n"]
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
