{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where
import           Client
import           Data.Maybe
import           GHC.MVar
import           Test.Hspec
import qualified TestContainers.Hspec as TC
import           WaitGroup

data Endpoints = Endpoints
                   { natsHost :: String
                   , natsPort :: Int
                   }

container :: TC.TestContainer Endpoints
container = do
    -- Launch the container image.
  natsContainer <- TC.run $ TC.containerRequest (TC.fromTag "nats:latest")
    -- Expose the port 4222 from within the container. The respective port
    -- on the host machine can be looked up using `containerPort` (see below).
    TC.& TC.setExpose [ 4222 ]
    -- Wait until the container is ready to accept requests. `run` blocks until
    -- readiness can be established.
    TC.& TC.setWaitingFor (TC.waitUntilMappedPortReachable 4222)
  pure $ Endpoints
    {
      natsHost = "0.0.0.0"
    , natsPort =
        TC.containerPort natsContainer 4222
    }

spec :: Spec
spec = do
  sys

sys = parallel $ do
  describe "client" $ do
    around (TC.withContainers container) $ do
      it "PING results in PONG" $ \(Endpoints natsHost natsPort) -> do
        socket <- defaultConn natsHost natsPort
        c <- newClient socket
        wg <- newWaitGroup 1
        ping c $ done wg
        wait wg
      it "messages are sent and received" $ \(Endpoints natsHost natsPort) -> do
        let topic = "SOME.TOPIC"
        let payload = "HELLO"
        socket <- defaultConn natsHost natsPort
        lock <- newEmptyMVar
        sidBox <- newEmptyMVar
        wg <- newWaitGroup 1
        assertClient <- newClient socket
        subscribe assertClient topic $ \msg -> do
          unsubscribe assertClient (sid msg)
          putMVar lock msg
          putMVar sidBox (sid msg)
          done wg
        socket' <- defaultConn natsHost natsPort
        promptClient <- newClient socket'
        publish promptClient topic [pubWithPayload payload]
        wait wg
        msg <- takeMVar lock
        sid' <- takeMVar sidBox
        msg `shouldBe` MsgView topic sid' Nothing (Just payload) Nothing
      it "replies are routed correctly" $ \(Endpoints natsHost natsPort) -> do
        let topic = "REQ.TOPIC"
        socket <- defaultConn natsHost natsPort
        remoteClient <- newClient socket
        subscribe remoteClient topic $ \msg -> do
          publish remoteClient (fromJust . replyTo $ msg) [pubWithPayload "WORLD"]
          unsubscribe remoteClient (sid msg)
        socket' <- defaultConn natsHost natsPort
        promptClient <- newClient socket'
        wg <- newWaitGroup 1
        publish promptClient topic [pubWithReplyCallback (\_ -> done wg), pubWithPayload "HELLO"]
        wait wg

