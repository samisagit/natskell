{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           API
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString        as BS
import qualified Data.Text              as Text
import           Data.Text.Encoding     (encodeUtf8)
import           Debug.Trace
import qualified Docker.Client          as DC
import           NatsWrappers
import           Test.Hspec
import           Text.Printf


spec :: Spec
spec = do
  sys

versions = ["latest", "2.9.8", "2.9.6"]

sys = parallel $ do
  forM_ versions $ \version ->
    describe (printf "client (nats:%s)" version) $ do
      around (withNATSConnection version) $ do
        it "sends and receives its own messages" $ \(_, host, port) -> do
          (c, asyncWait) <- testClient host port

          let keys = Prelude.map (packStr . show) [1 .. 100] :: [BS.ByteString]
          let comparisons = Prelude.map (\x msg -> compareMsg (Msg x x Nothing (Just x) Nothing) msg) keys
          asyncAsserts <- Prelude.mapM asyncAssert comparisons
          let keyedAssertions = Prelude.zip asyncAsserts keys

          sids <- forM keyedAssertions $ \((_, callback), x) -> do
            sid <- sub
              c
              [ subWithSubject x,
                subWithCallback callback
              ]
            takeMVar asyncWait
            return sid

          forM_ keyedAssertions $ \(_, x) -> do
            pub c [pubWithSubject x, pubWithPayload x]
            takeMVar asyncWait

          forM_ asyncAsserts $ \(lock, _) -> join . atomically $ takeTMVar lock

          forM_ sids $ \x -> do
            unsub c x
            takeMVar asyncWait

        it "receives others messages" $ \(_, host, port) -> do
          (c1, asyncWait1) <- testClient host port
          (c2, asyncWait2) <- testClient host port

          (lockAssure, assertAssure) <- asyncAssert (\_ -> return ())
          sid <- sub c1 [
            subWithSubject "foo",
            subWithCallback assertAssure
            ]
          takeMVar asyncWait1

          pub c2 [
            pubWithSubject "foo",
            pubWithPayload "bar"
            ]
          takeMVar asyncWait2
          join . atomically $ takeTMVar lockAssure
          unsub c1 sid
          takeMVar asyncWait1

        it "subscribes to its reply to" $ \(_, host, port) -> do
          (c1, asyncWait1) <- testClient host port
          (c2, asyncWait2) <- testClient host port

          -- when a message comes to foo with a replyTo, send a message to that subject
          sid <- sub c1 [
           subWithSubject "foo",
           subWithCallback (replyToReplyTo c2)
           ]
          takeMVar asyncWait1
          takeMVar asyncWait1

          (lockAssure, assertAssure) <- asyncAssert (\_ -> return ())
          pub c1 [
            pubWithSubject "foo",
            pubWithPayload "bar",
            pubWithReplyCallback assertAssure
            ]
          takeMVar asyncWait1
          takeMVar asyncWait2

          join . atomically $ takeTMVar lockAssure
          unsub c1 sid
          takeMVar asyncWait1

        it "receives others messages with headers" $ \(_, host, port) -> do
          (c1, asyncWait1) <- testClient host port
          (c2, asyncWait2) <- testClient host port

          (lockAssure, assertAssure) <- asyncAssert (\_ -> return ())
          sid <- sub c1 [
            subWithSubject "EXPECT.HEADERS",
            subWithCallback assertAssure
            ]
          takeMVar asyncWait1

          pub c2 [
            pubWithSubject "EXPECT.HEADERS",
            pubWithPayload "bar",
            pubWithHeaders [("foo", "bar"), ("more", "headers")]
            ]
          takeMVar asyncWait2
          join . atomically $ takeTMVar lockAssure
          unsub c1 sid
          takeMVar asyncWait1

compareMsg :: Msg -> Msg -> Expectation
compareMsg want got = do
  subject want `shouldBe` subject got
  replyTo want `shouldBe` replyTo got
  payload want `shouldBe` payload got
  headers want `shouldBe` headers got

replyToReplyTo client msg = do
  let reply = replyTo msg
  case reply of
    Just r -> pub client [pubWithSubject r, pubWithPayload "bar"]
    Nothing -> trace "expected replyTo field" error $ "expected replyTo in message " ++ show msg

testClient host port = do
  asyncWait <- newEmptyMVar
  nats <- connect host port
  c <- newClient nats [withAckCallback (putMVar asyncWait ())]
  handShake c
  return (c, asyncWait)

withNATSConnection :: Text.Text -> ((DC.ContainerID, String, Int) -> IO ()) -> IO ()
withNATSConnection tag = bracket (startNATS tag) stopNATS

asyncAssert :: (Msg -> Expectation) -> IO (TMVar Expectation, Msg -> IO ())
asyncAssert e = do
  lock <- newEmptyTMVarIO
  let callback msg = atomically $ putTMVar lock (e msg)
  return (lock, callback)

packStr :: String -> BS.ByteString
packStr = encodeUtf8 . Text.pack
