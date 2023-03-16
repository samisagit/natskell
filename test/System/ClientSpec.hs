{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           Client
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.Text              as Text
import qualified Docker.Client          as DC
import           NatsWrappers
import           Test.Hspec

spec :: Spec
spec = do
  sys

withNATSConnection :: Text.Text -> ((DC.ContainerID, String, Int) -> IO ()) -> IO ()
withNATSConnection tag = bracket (startNATS tag) stopNATS

versions = ["latest", "2.9.8", "2.9.6"]

sys = parallel $ do
  describe "client" $ do
    forM_ versions $ \version ->
      around (withNATSConnection version) $ do
        it "sends and receives messages" $ \(_, host, port) -> do
          nats <- connect host port
          handShake nats

          let wantA = Msg "foo" "sidA" Nothing (Just "a") Nothing
          let wantB = Msg "bar" "sidB" Nothing (Just "b") Nothing
          let wantC = Msg "baz" "sidC" Nothing (Just "c") Nothing

          (lockA, assertionA) <- asyncAssert $ \msg -> msg `shouldBe` wantA
          (lockB, assertionB) <- asyncAssert $ \msg -> msg `shouldBe` wantB
          (lockC, assertionC) <- asyncAssert $ \msg -> msg `shouldBe` wantC

          sub nats "sidA" "foo" $ \msg -> assertionA msg
          sub nats "sidB" "bar" $ \msg -> assertionB msg
          sub nats "sidC" "baz" $ \msg -> assertionC msg

          pub nats "foo" "a"
          pub nats "bar" "b"
          pub nats "baz" "c"

          join . atomically $ takeTMVar lockA
          join . atomically $ takeTMVar lockB
          join . atomically $ takeTMVar lockC

          unsub nats "sidA" "foo"
          unsub nats "sidB" "bar"
          unsub nats "sidC" "baz"

asyncAssert :: (Msg -> Expectation) -> IO (TMVar Expectation, Msg -> IO ())
asyncAssert e = do
  lock <- newEmptyTMVarIO
  let callback msg = atomically $ putTMVar lock (e msg)
  return (lock, callback)

