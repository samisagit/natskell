{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           Client
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.Text                    as Text
import qualified Docker.Client                as DC
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
        it "connects successfully" $ \(_, host, port) -> do
          nats <- connect host port

          let want = Msg "foo" "sid" Nothing (Just "bar") Nothing
          (lock, assertion) <- asyncAssert $ \msg -> msg `shouldBe` want
          sub nats "sid" "foo" $ \msg -> assertion msg
          pub nats "foo" "bar"
          join . atomically $ takeTMVar lock
          unsub nats "sid" "foo"

asyncAssert :: (Msg -> Expectation) -> IO (TMVar Expectation, Msg -> IO ())
asyncAssert e = do
  lock <- newEmptyTMVarIO
  let callback msg = atomically $ putTMVar lock (e msg)
  return (lock, callback)

