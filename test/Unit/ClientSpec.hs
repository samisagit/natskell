{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           Client
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString           as BS
import           Data.IORef
import           Harness
import qualified Nats.Nats                 as N
import           Test.Hspec
import           Transformers.Transformers
import           Types.Pub
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
      (lock, assertion, msg) <- matcherMsg matcher
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

    it "subscribes to its reply before publishing" $ do
      msgList <- newIORef [] :: IO (IORef [BS.ByteString])
      -- dump the msg onto the 'socket'
      socket <- newTMVarIO (""::BS.ByteString, msgList)
      nats <- N.nats socket
      pub nats [
        pubWithSubject "sub",
        pubWithPayload "payload",
        pubWithReplyCallback (\_ -> return ())
        ]
      readIORef msgList `shouldReturn` [
        transform $ Sub "sub.REPLY" Nothing "reply",
        transform $ Pub "sub" (Just "sub.REPLY") Nothing (Just "payload")
        ]
