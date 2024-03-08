{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           API
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
    Nothing -> trace "expected replyTo field" error $ "expected replyTo in message " ++ show msg

withNATSConnection :: Text.Text -> ((DC.ContainerID, String, Int) -> IO ()) -> IO ()
withNATSConnection tag = bracket (startNATS tag) stopNATS

versions = ["latest", "2.9.8", "2.9.6"]

sys = parallel $ do
  forM_ versions $ \version ->
    describe (printf "client (nats:%s)" version) $ do
      around (withNATSConnection version) $ do
        it "sends and receives its own messages" $ \(_, host, port) -> do
          nats <- connect host port
          c <- newClient nats True
          handShake c

          let keys = Prelude.map (packStr . show) [1 .. 100] :: [BS.ByteString]
          let comparisons = Prelude.map (\x msg -> compareMsg (Msg x x Nothing (Just x) Nothing) msg) keys
          asyncAsserts <- Prelude.mapM asyncAssert comparisons
          let keyedAssertions = Prelude.zip asyncAsserts keys

          sids <- forM keyedAssertions $ \((_, callback), x) ->
            sub
              c
              [ subWithSubject x,
                subWithCallback callback
              ]

          forM_ keyedAssertions $ \(_, x) -> pub c [pubWithSubject x, pubWithPayload x]

          forM_ asyncAsserts $ \(lock, _) -> join . atomically $ takeTMVar lock

          forM_ sids $ \x -> unsub c x

        it "receives others messages" $ \(_, host, port) -> do
          nats1 <- connect host port
          c1 <- newClient nats1 True
          handShake c1

          nats2 <- connect host port
          c2 <- newClient nats2 True
          handShake c2

          (lockAssure, assertAssure) <- asyncAssert (\_ -> return ())
          sid <- sub c1 [
            subWithSubject "foo",
            subWithCallback assertAssure
            ]

          pub c2 [
            pubWithSubject "foo",
            pubWithPayload "bar"
            ]
          join . atomically $ takeTMVar lockAssure
          unsub c1 sid

        it "subscribes to its reply to" $ \(_, host, port) -> do
          nats1 <- connect host port
          c1 <- newClient nats1 True
          handShake c1

          nats2 <- connect host port
          c2 <- newClient nats2 True
          handShake c2

          -- when a message comes to foo with a replyTo, send a message to that subject
          sid <- sub c1 [
           subWithSubject "foo",
           subWithCallback (replyToReplyTo c2)
           ]

          (lockAssure, assertAssure) <- asyncAssert (\_ -> return ())
          pub c1 [
            pubWithSubject "foo",
            pubWithPayload "bar",
            pubWithReplyCallback assertAssure
            ]

          join . atomically $ takeTMVar lockAssure
          unsub c1 sid

asyncAssert :: (Msg -> Expectation) -> IO (TMVar Expectation, Msg -> IO ())
asyncAssert e = do
  lock <- newEmptyTMVarIO
  let callback msg = atomically $ putTMVar lock (e msg)
  return (lock, callback)

packStr :: String -> BS.ByteString
packStr = encodeUtf8 . Text.pack
