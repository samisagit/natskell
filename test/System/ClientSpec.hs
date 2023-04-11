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
        it "sends and receives it's own messages" $ \(_, host, port) -> do
          nats <- connect host port
          handShake nats

          let keys = Prelude.map (packStr . show) [1..100] :: [BS.ByteString]
          let subData = Prelude.map (\x msg -> msg `shouldBe` Msg x x Nothing (Just x) Nothing) keys
          asyncAsserts <- Prelude.mapM asyncAssert subData
          let keyedAssertions = Prelude.zip asyncAsserts keys

          forM_ keyedAssertions $ \((_, callback), x) -> sub nats x x callback

          forM_ keyedAssertions $ \(_, x) -> pub nats x x

          forM_ asyncAsserts $ \(lock, _) -> atomically $ takeTMVar lock

          forM_ keyedAssertions $ \(_, x) -> unsub nats x x

asyncAssert :: (Msg -> Expectation) -> IO (TMVar Expectation, Msg -> IO ())
asyncAssert e = do
  lock <- newEmptyTMVarIO
  let callback msg = atomically $ putTMVar lock (e msg)
  return (lock, callback)

packStr :: String -> BS.ByteString
packStr = encodeUtf8 . Text.pack
