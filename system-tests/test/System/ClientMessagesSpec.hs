{-# LANGUAGE OverloadedStrings #-}

module ClientMessagesSpec (spec) where

import           Client
import           Control.Concurrent
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec =
  clientSystemTest "messages are sent and received" $ \logger (Endpoints natsHost natsPort) -> do
    let topic = "SOME.TOPIC"
    let payload = "HELLO"
    lock <- newEmptyMVar
    sidBox <- newEmptyMVar
    wg <- newWaitGroup 1
    assertClient <- newClient [(natsHost, natsPort)]
      [ withConnectName "0dfe787e-383b-4cb8-a73f-8474f4cc0497"
      , withLoggerConfig logger
      ]
    promptClient <- newClient [(natsHost, natsPort)]
      [ withConnectName "0e81e61a-932f-4036-9cdd-9a65fb4ed829"
      , withLoggerConfig logger
      ]
    subscribe assertClient topic [withFlush] $ \msg -> do
      case msg of
        Nothing -> error "Received empty message"
        Just msg' -> do
          unsubscribe assertClient (sid msg')
          putMVar lock msg'
          putMVar sidBox (sid msg')
          done wg
    publish promptClient topic [withPayload payload]
    wait wg
    msg <- takeMVar lock
    sid' <- takeMVar sidBox
    msg `shouldBe` MsgView topic sid' Nothing (Just payload) Nothing
    close assertClient
    close promptClient
