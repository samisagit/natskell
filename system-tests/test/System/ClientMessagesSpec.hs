{-# LANGUAGE OverloadedStrings #-}

module ClientMessagesSpec (spec) where

import           API
    ( Client (..)
    , MsgView (..)
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

messageTest :: Maybe BS.ByteString -> [ConfigOption] -> Endpoints -> IO ()
messageTest payload loggerOptions (Endpoints natsHost natsPort) = do
  let topic = "SOME.TOPIC"
  lock <- newEmptyMVar
  sidBox <- newEmptyMVar
  wg <- newWaitGroup 1
  assertClient <- newClient [(natsHost, natsPort)] $
    withConnectName "0dfe787e-383b-4cb8-a73f-8474f4cc0497"
      : loggerOptions
  promptClient <- newClient [(natsHost, natsPort)] $
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
  assertClientA <- newClient [(natsHost, natsPort)] $
    withConnectName "30d02187-f1c7-4b60-b4fd-3633d381b9c8"
      : loggerOptions
  assertClientB <- newClient [(natsHost, natsPort)] $
    withConnectName "7e2cc0b7-4d6d-4ad5-aa33-eeab0e7083c9"
      : loggerOptions
  promptClient <- newClient [(natsHost, natsPort)] $
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
