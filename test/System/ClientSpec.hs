{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           API
import           CallbackAssertions
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.Text              as Text
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
        it "receives others messages" $ \(_, host, port) -> do
          (c1, asyncWait1) <- testClient host port
          (c2, asyncWait2) <- testClient host port
          let subj = "foo"
          let payload = "bar"

          (lockAssure, assertAssure) <- asyncAssert [
            subjectAssertion subj,
            payloadAssertion $ Just payload,
            headerAssertion Nothing
            ]
          sid <- sub c1 [
            subWithSubject subj,
            subWithCallback assertAssure
            ]
          takeMVar asyncWait1

          pub c2 [
            pubWithSubject subj,
            pubWithPayload payload
            ]
          takeMVar asyncWait2
          join . atomically $ takeTMVar lockAssure
          unsub c1 sid
          takeMVar asyncWait1

        it "subscribes to its reply to" $ \(_, host, port) -> do
          (c1, asyncWait1) <- testClient host port
          (c2, asyncWait2) <- testClient host port
          let subj = "foo"
          let payload = "bar"

          -- when a message comes to foo with a replyTo, send a message to that subject
          sid <- sub c1 [
           subWithSubject subj,
           subWithCallback (replyToReplyTo c2)
           ]
          takeMVar asyncWait1
          takeMVar asyncWait1

          (lockAssure, assertAssure) <- asyncAssert [
            payloadAssertion $ Just payload,
            headerAssertion Nothing
            ]
          pub c1 [
            pubWithSubject subj,
            pubWithPayload payload,
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

          let subj = "EXPECT.HEADERS"
          let payload = "bar"
          let headers = [("foo", "bar"), ("more", "headers")]
          (lockAssure, assertAssure) <- asyncAssert [
            payloadAssertion $ Just payload,
            subjectAssertion subj,
            headerAssertion $ Just headers
            ]
          sid <- sub c1 [
            subWithSubject subj,
            subWithCallback assertAssure
            ]
          takeMVar asyncWait1

          pub c2 [
            pubWithSubject subj,
            pubWithPayload payload,
            pubWithHeaders headers
            ]
          takeMVar asyncWait2
          join . atomically $ takeTMVar lockAssure
          unsub c1 sid
          takeMVar asyncWait1

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
