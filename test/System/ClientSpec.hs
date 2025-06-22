{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where
import           Client
import           Control.Exception
import           Control.Monad     (forM_)
import           Data.Maybe
import qualified Data.Text         as Text
import qualified Docker.Client     as DC
import           GHC.MVar
import           NatsWrappers
import           Test.Hspec
import           Text.Printf       (printf)
import           WaitGroup

spec :: Spec
spec = do
  sys

versions = ["latest", "2.9.8", "2.9.6"]

sys = parallel $ do
  forM_ versions $ \version ->
    describe (printf "client (nats:%s)" version) $ do
      around (withNATSConnection version) $ do
        it "PING results in PONG" $ \(_, host, port) -> do
          socket <- defaultConn host port
          c <- newClient socket
          wg <- newWaitGroup 1
          ping c $ done wg
          wait wg
        it "messages are sent and received" $ \(_, host, port) -> do
          let topic = "SOME.TOPIC"
          let payload = "HELLO"
          socket <- defaultConn host port
          lock <- newEmptyMVar
          sidBox <- newEmptyMVar
          wg <- newWaitGroup 1
          assertClient <- newClient socket
          subscribe assertClient topic $ \msg -> do
            unsubscribe assertClient (sid msg)
            putMVar lock msg
            putMVar sidBox (sid msg)
            done wg
          socket' <- defaultConn host port
          promptClient <- newClient socket'
          publish promptClient topic [pubWithPayload payload]
          wait wg
          msg <- takeMVar lock
          sid' <- takeMVar sidBox
          msg `shouldBe` Msg topic sid' Nothing (Just payload) Nothing
        it "replies are routed correctly" $ \(_, host, port) -> do
          let topic = "REQ.TOPIC"
          socket <- defaultConn host port
          remoteClient <- newClient socket
          subscribe remoteClient topic $ \msg -> do
            publish remoteClient (fromJust . replyTo $ msg) [pubWithPayload "WORLD"]
            unsubscribe remoteClient (sid msg)
          socket' <- defaultConn host port
          promptClient <- newClient socket'
          wg <- newWaitGroup 1
          publish promptClient topic [pubWithReplyCallback (\_ -> done wg), pubWithPayload "HELLO"]
          wait wg

withNATSConnection :: Text.Text -> ((DC.ContainerID, String, Int) -> IO ()) -> IO ()
withNATSConnection tag = bracket (startNATS tag) stopNATS
