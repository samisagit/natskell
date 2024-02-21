{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           Client
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString        as BS
import qualified Data.Text              as Text
import           Data.Text.Encoding     (encodeUtf8)
import qualified Docker.Client          as DC
import           NatsWrappers
import           Test.Hspec
import           Text.Printf


spec :: Spec
spec = do
  sys

compareMsg :: Msg -> Msg -> Expectation
compareMsg want got = do
  subject want `shouldBe` subject got
  replyTo want `shouldBe` replyTo got
  payload want `shouldBe` payload got
  headers want `shouldBe` headers got

replyToReplyTo nats msg = do
  let reply = replyTo msg
  case reply of
    Just r -> pub nats [pubWithSubject r, pubWithPayload "bar"]
    Nothing -> error $ "expected replyTo in message " ++ show msg

withNATSConnection :: Text.Text -> ((DC.ContainerID, String, Int) -> IO ()) -> IO ()
withNATSConnection tag = bracket (startNATS tag) stopNATS

versions = ["latest", "2.9.8", "2.9.6"]

sys = parallel $ do
  forM_ versions $ \version ->
    describe (printf "client (nats:%s)" version) $ do
      around (withNATSConnection version) $ do
        it "sends and receives its own messages" $ \(_, host, port) -> do
          nats <- connect host port
          handShake nats

          let keys = Prelude.map (packStr . show) [1 .. 100] :: [BS.ByteString]
          let comparisons = Prelude.map (\x msg -> compareMsg (Msg x x Nothing (Just x) Nothing) msg) keys
          asyncAsserts <- Prelude.mapM asyncAssert comparisons
          let keyedAssertions = Prelude.zip asyncAsserts keys

          forM_ keyedAssertions $ \((_, callback), x) ->
            sub
              nats
              [ subWithSubject x,
                subWithCallback callback
              ]

          forM_ keyedAssertions $ \(_, x) -> pub nats [pubWithSubject x, pubWithPayload x]

          forM_ asyncAsserts $ \(lock, _) -> join . atomically $ takeTMVar lock

          forM_ keyedAssertions $ \(_, x) -> unsub nats x x

        it "receives others messages" $ \(_, host, port) -> do
          nats1 <- connect host port
          handShake nats1

          nats2 <- connect host port
          handShake nats2

          (lockAssure, assertAssure) <- asyncAssert (\_ -> return ())
          sub nats1 [
            subWithSubject "foo",
            subWithCallback assertAssure
            ]

          pub nats2 [
            pubWithSubject "foo",
            pubWithPayload "bar"
            ]
          join . atomically $ takeTMVar lockAssure

        it "subscribes to its reply to" $ \(_, host, port) -> do
          nats1 <- connect host port
          handShake nats1

          nats2 <- connect host port
          handShake nats2

          -- when a message comes to foo with a replyTo, send a message to that subject
          sub nats1 [
           subWithSubject "foo",
           subWithCallback (replyToReplyTo nats2)
           ]

          (lockAssure, assertAssure) <- asyncAssert (\_ -> return ())
          pub nats1 [
            pubWithSubject "foo",
            pubWithPayload "bar",
            pubWithReplyCallback assertAssure
            ]

          join . atomically $ takeTMVar lockAssure

asyncAssert :: (Msg -> Expectation) -> IO (TMVar Expectation, Msg -> IO ())
asyncAssert e = do
  lock <- newEmptyTMVarIO
  let callback msg = atomically $ putTMVar lock (e msg)
  return (lock, callback)

packStr :: String -> BS.ByteString
packStr = encodeUtf8 . Text.pack
