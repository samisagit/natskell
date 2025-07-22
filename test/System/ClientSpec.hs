{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where
import           Client
import qualified Data.ByteString      as BS
import           Data.Maybe
import           GHC.MVar
import           Test.Hspec
import           TestContainers
import qualified TestContainers.Hspec as TC
import           WaitGroup

data Endpoints = Endpoints
                   { natsHost :: String
                   , natsPort :: Int
                   }

fileLogConsumer :: FilePath -> LogConsumer
fileLogConsumer fp pipe line = do
  case pipe of
    Stdout -> do
      BS.appendFile fp line
      BS.appendFile fp "\r\n"
    Stderr -> do
      BS.appendFile fp line
      BS.appendFile fp "\r\n"

breakLogFile :: FilePath -> IO ()
breakLogFile fp = do
  BS.appendFile fp "\r\n"
  BS.appendFile fp "\r\n"
  BS.appendFile fp "--------------------"
  BS.appendFile fp "\r\n"
  BS.appendFile fp "\r\n"


container :: TC.TestContainer Endpoints
container = do
    -- Launch the container image.
  natsContainer <- TC.run (TC.containerRequest
    (TC.fromTag "nats:latest")
    -- Expose the port 4222 from within the container. The respective port
    -- on the host machine can be looked up using `containerPort` (see below).
    TC.& TC.setExpose [ 4222 ]
    -- Wait until the container is ready to accept requests. `run` blocks until
    -- readiness can be established.
    TC.& TC.setWaitingFor (TC.waitUntilMappedPortReachable 4222)
    -- Set the arguments required to debug output, then follow the logs.
    TC.& TC.setCmd ["-DV"]
    TC.& withFollowLogs (fileLogConsumer "nats.log")
    )

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
    around_ (\action -> action >> breakLogFile "nats.log") $ do
      around (TC.withContainers container) $ do
        let caseName = "PING results in PONG" :: BS.ByteString
          in
          it (show caseName) $ \(Endpoints natsHost natsPort) -> do
            c <- newClient [(natsHost, natsPort)] [withConnectName caseName]
            wg <- newWaitGroup 1
            ping c $ done wg
            wait wg
        let caseName = "messages are sent and received" :: BS.ByteString
          in
          it (show caseName) $ \(Endpoints natsHost natsPort) -> do
            let topic = "SOME.TOPIC"
            let payload = "HELLO"
            lock <- newEmptyMVar
            sidBox <- newEmptyMVar
            wg <- newWaitGroup 1
            assertClient <- newClient [(natsHost, natsPort)] [withConnectName caseName]
            subscribe assertClient topic $ \msg -> do
              unsubscribe assertClient (sid msg)
              putMVar lock msg
              putMVar sidBox (sid msg)
              done wg
            promptClient <- newClient [(natsHost, natsPort)] [withConnectName caseName]
            publish promptClient topic [pubWithPayload payload]
            wait wg
            msg <- takeMVar lock
            sid' <- takeMVar sidBox
            msg `shouldBe` MsgView topic sid' Nothing (Just payload) Nothing
        let caseName = "replies are routed correctly" :: BS.ByteString
          in
          it (show caseName) $ \(Endpoints natsHost natsPort) -> do
          let topic = "REQ.TOPIC"
          remoteClient <- newClient [(natsHost, natsPort)] [withConnectName caseName]
          subscribe remoteClient topic $ \msg -> do
            publish remoteClient (fromJust . replyTo $ msg) [pubWithPayload "WORLD"]
            unsubscribe remoteClient (sid msg)
          promptClient <- newClient [(natsHost, natsPort)] [withConnectName caseName]
          wg <- newWaitGroup 1
          publish promptClient topic [pubWithReplyCallback (\_ -> done wg), pubWithPayload "HELLO"]
          wait wg

