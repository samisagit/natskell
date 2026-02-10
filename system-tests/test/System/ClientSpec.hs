{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           Client
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Data.ByteString        as BS
import           Data.Maybe
import           Test.Hspec
import           TestContainers         hiding (exitCode)
import qualified TestContainers.Hspec   as TC
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

testLoggerConfig :: IO LoggerConfig
testLoggerConfig = do
  lock <- newTMVarIO ()
  pure $ LoggerConfig Debug (\_ s -> putStrLn s) lock

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
    logger <- runIO testLoggerConfig
    around_ (\action -> action >> breakLogFile "nats.log") $ do
      around (TC.withContainers container) $ do
        it "PING results in PONG"  $ \(Endpoints natsHost natsPort) -> do
          c <- newClient [(natsHost, natsPort)] [withConnectName "1f27aec6-e832-41ad-88ad-15555985b754", withLoggerConfig logger]
          wg <- newWaitGroup 1
          ping c $ done wg
          wait wg
          close c
        it "exits immediately on fatal error" $ \(Endpoints _ _) -> do
          wg <- newWaitGroup 1
          exitResult <- newEmptyTMVarIO
          _ <- newClient [("0.0.0.0", 4999)] [
            withConnectName "fatal-reset-test",
            withExitAction (\r -> atomically (putTMVar exitResult r) >> done wg),
            withLoggerConfig logger
            ]
          wait wg
          result <- atomically $ readTMVar exitResult
          exitStatus result `shouldBe` ExitStatusRetryExhausted
          exitCode result `shouldBe` 1
          case exitReason result of
            ExitRetriesExhausted _ -> pure ()
            other                  -> expectationFailure $ "Unexpected exit reason: " ++ show other
        it "user can close connection" $ \(Endpoints natsHost natsPort) -> do
          wg <- newWaitGroup 1
          exitResult <- newEmptyTMVarIO
          client <- newClient [(natsHost, natsPort)] [
            withConnectName "b9ed73e3-9674-41a2-9979-bb63b78c6579",
            withExitAction (\r -> atomically (putTMVar exitResult r) >> done wg),
            withLoggerConfig logger
            ]
          close client
          wait wg
          result <- atomically $ readTMVar exitResult
          exitReason result `shouldBe` ExitClosedByUser
          exitCode result `shouldBe` 0
        it "messages are sent and received"  $ \(Endpoints natsHost natsPort) -> do
          let topic = "SOME.TOPIC"
          let payload = "HELLO"
          lock <- newEmptyMVar
          sidBox <- newEmptyMVar
          wg <- newWaitGroup 1
          assertClient <- newClient [(natsHost, natsPort)] [withConnectName "0dfe787e-383b-4cb8-a73f-8474f4cc0497", withLoggerConfig logger]
          subscribe assertClient topic $ \msg -> do
            case msg of
              Nothing -> error "Received empty message"
              Just msg -> do
                unsubscribe assertClient (sid msg)
                putMVar lock msg
                putMVar sidBox (sid msg)
                done wg
          promptClient <- newClient [(natsHost, natsPort)] [withConnectName "0e81e61a-932f-4036-9cdd-9a65fb4ed829", withLoggerConfig logger]
          publish promptClient topic [withPayload payload]
          wait wg
          msg <- takeMVar lock
          sid' <- takeMVar sidBox
          msg `shouldBe` MsgView topic sid' Nothing (Just payload) Nothing
          close assertClient
          close promptClient
        it "replies are routed correctly"  $ \(Endpoints natsHost natsPort) -> do
          let topic = "REQ.TOPIC"
          remoteClient <- newClient [(natsHost, natsPort)] [withConnectName "6eff2527-1ad5-4b0c-b4e5-4a52a7d17639", withLoggerConfig logger]
          subscribe remoteClient topic $ \msg -> do
            case msg of
              Nothing -> error "Received empty message"
              Just msg -> do
                publish remoteClient (fromJust . replyTo $ msg) [withPayload "WORLD"]
                unsubscribe remoteClient (sid msg)
          promptClient <- newClient [(natsHost, natsPort)] [withConnectName "6eff2527-1ad5-4b0c-b4e5-4a52a7d17639", withLoggerConfig logger]
          wg <- newWaitGroup 1
          publish promptClient topic [withReplyCallback (\_ -> done wg), withPayload "HELLO"]
          wait wg
          close remoteClient
          close promptClient
        it "cycles through servers"  $ \(Endpoints natsHost natsPort) -> do
          c <- newClient [("0.0.0.0", 4999), (natsHost, natsPort)] [withConnectName "b896f0fb-ea45-4456-86d9-b7d6269eb75f", withLoggerConfig logger, withConnectionAttempts 2]
          wg <- newWaitGroup 1
          ping c $ done wg
          wait wg
          close c
        it "exits when no valid servers" $ \(Endpoints _ _) -> do
          wg <- newWaitGroup 1
          exitResult <- newEmptyTMVarIO
          newClient [("0.0.0.0", 4999)] [
            withConnectName "9b694d4e-7b78-459c-9126-57e582564a0b",
            withExitAction (\r -> atomically (putTMVar exitResult r) >> done wg),
            withLoggerConfig logger
            ]
          wait wg
          result <- atomically $ readTMVar exitResult
          exitStatus result `shouldBe` ExitStatusRetryExhausted
          exitCode result `shouldBe` 1
          case exitReason result of
            ExitRetriesExhausted _ -> pure ()
            other                  -> expectationFailure $ "Unexpected exit reason: " ++ show other
