{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           Client
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.ByteString        as BS
import qualified Data.Text              as Text
import           Data.Text.Encoding     (encodeUtf8)
import           Data.UUID
import           Data.UUID.V4
import qualified Docker.Client          as DC
import           NatsWrappers
import           System.Timeout
import           Test.Hspec
import           Text.Printf

sidService () = toASCIIBytes <$> nextRandom

spec :: Spec
spec = do
  sys

withNATSConnection :: Text.Text -> ((DC.ContainerID, String, Int) -> IO ()) -> IO ()
withNATSConnection tag = bracket (startNATS tag) stopNATS

versions = ["latest", "2.9.8", "2.9.6"]

sys = parallel $ do
  forM_ versions $ \version ->
    describe (printf "client (nats:%s)" version) $ do
      around (withNATSConnection version) $ do
        it "sends and receives its own messages" $ \(_, host, port) -> do
          nats <- connect host port sidService
          handShake nats

          let keys = Prelude.map (packStr . show) [1 .. 100] :: [BS.ByteString]
          let subData = Prelude.map (\x msg -> msg `shouldBe` Msg x x Nothing (Just x) Nothing) keys
          asyncAsserts <- Prelude.mapM asyncAssert subData
          let keyedAssertions = Prelude.zip asyncAsserts keys

          forM_ keyedAssertions $ \((_, callback), x) ->
            sub
              nats
              [ subWithSubject x,
                subWithSID x,
                subWithCallback callback
              ]

          forM_ keyedAssertions $ \(_, x) -> pub nats [pubWithSubject x, pubWithPayload x]

          forM_ asyncAsserts $ \(lock, _) -> join . atomically $ takeTMVar lock

          forM_ keyedAssertions $ \(_, x) -> unsub nats x x

        it "handles replies" $ \(_, host, port) -> do
          nats1 <- connect host port sidService
          handShake nats1

          nats2 <- connect host port sidService
          handShake nats2

          (lockAssure, assertAssure) <- asyncAssert (\_ -> return ())
          sub nats2 [subWithSubject "foo", subWithSID "xyz", subWithCallback assertAssure]

          -- wait for the inital sub to go through
          -- TODO: could make use of OK responses...
          threadDelay 1000000

          (lock, assertion) <- asyncAssert (\msg -> msg `shouldBe` Msg "foo.REPLY" "abc" Nothing (Just "bar") Nothing)
          pub nats1 [pubWithSubject "foo", pubWithPayload "bar", pubWithReplyCallback assertion]

          -- ensure the inital pub is received
          timeout 1000000 (join . atomically $ takeTMVar lockAssure) `shouldNotReturn` Nothing

          -- mock a response from nats2
          pub nats2 [pubWithSubject "foo.REPLY", pubWithPayload "bar"]

          -- wait for a response from nats2
          timeout 1000000 (join . atomically $ takeTMVar lock) `shouldNotReturn` Nothing

asyncAssert :: (Msg -> Expectation) -> IO (TMVar Expectation, Msg -> IO ())
asyncAssert e = do
  lock <- newEmptyTMVarIO
  let callback msg = atomically $ putTMVar lock (e msg)
  return (lock, callback)

packStr :: String -> BS.ByteString
packStr = encodeUtf8 . Text.pack
