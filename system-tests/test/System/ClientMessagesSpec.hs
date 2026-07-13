{-# LANGUAGE OverloadedStrings #-}

module ClientMessagesSpec (spec) where

import           API
    ( Client (..)
    , MsgView (..)
    , withHeaders
    , withPayload
    , withQueueGroup
    )
import           Client
import           Control.Concurrent
import           Control.Exception  (finally)
import           Control.Monad      (void, when)
import qualified Data.ByteString    as BS
import           Data.Maybe         (fromMaybe)
import           System.Timeout     (timeout)
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec = do
  clientSystemTest "2aa35ca5-5e75-4a47-b3af-97dbe2881151" "messages are sent and received" $
    messageTest (Just "HELLO")
  clientSystemTest "9f24cb8e-c4a1-4fe5-9e87-dbb0bc3f79a2" "messages without payload are sent and received" $
    messageTest Nothing
  clientSystemTest "5b755d41-8f7a-4f5e-a336-5c4b97341b84" "queue group subscriptions distribute messages" queueGroupTest
  clientSystemTest "f5d97b1d-ece6-4682-9343-995ee8db0c3b" "normal subscriptions survive reconnect" normalSubscriptionReconnectTest
  clientSystemTest "e242fd0f-4134-4279-a6cc-23d49a2f58fb" "queue subscriptions survive reconnect" queueSubscriptionReconnectTest
  clientSystemTest "2c6c1121-97e4-41e3-9663-1b2c66850a4e" "headers preserve case and duplicate values" headersRoundTripTest

messageTest :: Maybe BS.ByteString -> [ConfigOption] -> Endpoints -> IO ()
messageTest payload loggerOptions (Endpoints natsHost natsPort) = do
  let topic = "SOME.TOPIC"
  lock <- newEmptyMVar
  sidBox <- newEmptyMVar
  wg <- newWaitGroup 1
  assertClient <- newTestClient [(natsHost, natsPort)] $
    withConnectName "0dfe787e-383b-4cb8-a73f-8474f4cc0497"
      : loggerOptions
  promptClient <- newTestClient [(natsHost, natsPort)] $
    withConnectName "0e81e61a-932f-4036-9cdd-9a65fb4ed829"
      : loggerOptions
  subscribe assertClient topic [] $ \msg -> do
    case msg of
      Nothing -> error "Received empty message"
      Just msg' -> do
        unsubscribe assertClient (sid msg')
        putMVar lock msg'
        putMVar sidBox (sid msg')
        done wg
  flush assertClient
  let publishOptions = maybe [] (\payloadValue -> [withPayload payloadValue]) payload
  publish promptClient topic publishOptions
  wait wg
  msg <- takeMVar lock
  sid' <- takeMVar sidBox
  msg `shouldBe` MsgView topic sid' Nothing payload Nothing
  close assertClient
  close promptClient

queueGroupTest :: [ConfigOption] -> Endpoints -> IO ()
queueGroupTest loggerOptions (Endpoints natsHost natsPort) = do
  let topic = "QUEUE.TOPIC"
      queueGroup = "WORKERS"
      payloads = ["job-1", "job-2", "job-3", "job-4", "job-5"]
      expectedCount = length payloads
  received <- newMVar []
  expectedDone <- newEmptyMVar
  extraDelivery <- newEmptyMVar
  assertClientA <- newTestClient [(natsHost, natsPort)] $
    withConnectName "30d02187-f1c7-4b60-b4fd-3633d381b9c8"
      : loggerOptions
  assertClientB <- newTestClient [(natsHost, natsPort)] $
    withConnectName "7e2cc0b7-4d6d-4ad5-aa33-eeab0e7083c9"
      : loggerOptions
  promptClient <- newTestClient [(natsHost, natsPort)] $
    withConnectName "4185b922-9a1c-4c09-82e6-8f6f06ed434b"
      : loggerOptions
  let cleanup = do
        close assertClientA
        close assertClientB
        close promptClient
      record body = do
        count <- modifyMVar received $ \bodies -> do
          let bodies' = body : bodies
          pure (bodies', length bodies')
        when (count == expectedCount) $
          void (tryPutMVar expectedDone ())
        when (count > expectedCount) $
          void (tryPutMVar extraDelivery ())
      handle msg =
        case msg of
          Nothing   -> record "<empty>"
          Just msg' -> record (fromMaybe "<empty payload>" (payload msg'))
  (do
      _ <- subscribe assertClientA topic [withQueueGroup queueGroup] handle
      _ <- subscribe assertClientB topic [withQueueGroup queueGroup] handle
      flush assertClientA
      flush assertClientB
      mapM_ (\body -> publish promptClient topic [withPayload body]) payloads
      flush promptClient
      delivered <- timeout (5 * 1000000) (takeMVar expectedDone)
      case delivered of
        Nothing -> expectationFailure "queue group did not receive every message"
        Just () -> pure ()
      extra <- timeout 500000 (takeMVar extraDelivery)
      case extra of
        Nothing -> pure ()
        Just () -> expectationFailure "queue group delivered a message more than once"
      receivedPayloads <- readMVar received
      receivedPayloads `shouldMatchList` payloads)
    `finally` cleanup

normalSubscriptionReconnectTest :: [ConfigOption] -> Endpoints -> IO ()
normalSubscriptionReconnectTest loggerOptions (Endpoints natsHost natsPort) = do
  let topic = "RECONNECT.NORMAL"
  received <- newEmptyMVar
  subscriber <- newTestClient [(natsHost, natsPort)] $
    withConnectName "reconnect-normal-subscriber"
      : loggerOptions
  publisher <- newTestClient [(natsHost, natsPort)] $
    withConnectName "reconnect-normal-publisher"
      : loggerOptions
  let cleanup = do
        close subscriber
        close publisher
  (do
      _ <- subscribe subscriber topic [] (putMVar received)
      flush subscriber
      reset subscriber
      flush subscriber
      publish publisher topic [withPayload "after-reconnect"]
      flush publisher
      delivery <- timeout (5 * 1000000) (takeMVar received)
      case delivery of
        Nothing ->
          expectationFailure "normal subscription did not receive after reconnect"
        Just Nothing ->
          expectationFailure "normal subscription received an empty callback"
        Just (Just msg) ->
          payload msg `shouldBe` Just "after-reconnect")
    `finally` cleanup

queueSubscriptionReconnectTest :: [ConfigOption] -> Endpoints -> IO ()
queueSubscriptionReconnectTest loggerOptions (Endpoints natsHost natsPort) = do
  let topic = "RECONNECT.QUEUE"
      queueGroup = "RECONNECT-WORKERS"
  received <- newEmptyMVar
  subscriber <- newTestClient [(natsHost, natsPort)] $
    withConnectName "reconnect-queue-subscriber"
      : loggerOptions
  publisher <- newTestClient [(natsHost, natsPort)] $
    withConnectName "reconnect-queue-publisher"
      : loggerOptions
  let cleanup = do
        close subscriber
        close publisher
  (do
      _ <- subscribe subscriber topic [withQueueGroup queueGroup] (putMVar received)
      flush subscriber
      reset subscriber
      flush subscriber
      publish publisher topic [withPayload "queued-after-reconnect"]
      flush publisher
      delivery <- timeout (5 * 1000000) (takeMVar received)
      case delivery of
        Nothing ->
          expectationFailure "queue subscription did not receive after reconnect"
        Just Nothing ->
          expectationFailure "queue subscription received an empty callback"
        Just (Just msg) ->
          payload msg `shouldBe` Just "queued-after-reconnect")
    `finally` cleanup

headersRoundTripTest :: [ConfigOption] -> Endpoints -> IO ()
headersRoundTripTest loggerOptions (Endpoints natsHost natsPort) = do
  let topic = "HEADERS.ROUNDTRIP"
      expectedHeaders =
        [ ("X-Test", "one")
        , ("x-test", "two")
        , ("X-Multi", "red")
        , ("X-Multi", "blue")
        ]
  received <- newEmptyMVar
  subscriber <- newTestClient [(natsHost, natsPort)] $
    withConnectName "headers-round-trip-subscriber"
      : loggerOptions
  publisher <- newTestClient [(natsHost, natsPort)] $
    withConnectName "headers-round-trip-publisher"
      : loggerOptions
  let cleanup = do
        close subscriber
        close publisher
  (do
      _ <- subscribe subscriber topic [] (putMVar received)
      flush subscriber
      publish publisher topic [withHeaders expectedHeaders, withPayload "header-payload"]
      flush publisher
      delivery <- timeout (5 * 1000000) (takeMVar received)
      case delivery of
        Nothing ->
          expectationFailure "header message was not delivered"
        Just Nothing ->
          expectationFailure "header subscription received an empty callback"
        Just (Just msg) -> do
          payload msg `shouldBe` Just "header-payload"
          headers msg `shouldBe` Just expectedHeaders)
    `finally` cleanup
