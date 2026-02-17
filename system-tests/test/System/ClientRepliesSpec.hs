{-# LANGUAGE OverloadedStrings #-}

module ClientRepliesSpec (spec) where

import           Client
import           Control.Concurrent.STM
import           Data.Maybe
import           Test.Hspec
import           TestSupport

spec :: Spec
spec =
  clientSystemTest "replies are routed correctly" $ \logger (Endpoints natsHost natsPort) -> do
    let topic = "REQ.TOPIC"
    remoteClient <- newClient [(natsHost, natsPort)]
      [ withConnectName "6eff2527-1ad5-4b0c-b4e5-4a52a7d17639"
      , withLoggerConfig logger
      ]
    promptClient <- newClient [(natsHost, natsPort)]
      [ withConnectName "6eff2527-1ad5-4b0c-b4e5-4a52a7d17639"
      , withLoggerConfig logger
      ]
    subscribe remoteClient topic [] $ \msg -> do
      case msg of
        Nothing -> error "Received empty message"
        Just msg' -> do
          publish remoteClient (fromJust . replyTo $ msg') [withPayload "WORLD"]
          unsubscribe remoteClient (sid msg')
    replyBox <- newEmptyTMVarIO
    publish promptClient topic [withReplyCallback (atomically . putTMVar replyBox), withPayload "HELLO"]
    reply <- atomically $ readTMVar replyBox
    case reply of
      Nothing   -> expectationFailure "Expected a reply but got Nothing"
      Just msg' -> payload msg' `shouldBe` Just "WORLD"
    close remoteClient
    close promptClient
