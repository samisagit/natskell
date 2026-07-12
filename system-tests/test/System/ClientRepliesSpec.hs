{-# LANGUAGE OverloadedStrings #-}

module ClientRepliesSpec (spec) where

import           API
    ( Client (..)
    , MsgView (..)
    , withPayload
    , withReplyCallback
    )
import           Client
import           Control.Concurrent.STM
import           Control.Exception      (finally)
import           Data.Maybe
import           System.Timeout
import           Test.Hspec
import           TestSupport

spec :: Spec
spec = do
  clientSystemTest "79b1247b-10b6-4c1c-8d78-20a9e9f30cc0" "replies are routed correctly" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    let topic = "REQ.TOPIC"
    remoteClient <- newClient [(natsHost, natsPort)] $
      withConnectName "6eff2527-1ad5-4b0c-b4e5-4a52a7d17639"
        : loggerOptions
    promptClient <- newClient [(natsHost, natsPort)] $
      withConnectName "6eff2527-1ad5-4b0c-b4e5-4a52a7d17639"
        : loggerOptions
    subscribe remoteClient topic [] $ \msg -> do
      case msg of
        Nothing -> error "Received empty message"
        Just msg' -> do
          publish remoteClient (fromJust . replyTo $ msg') [withPayload "WORLD"]
          unsubscribe remoteClient (sid msg')
    flush remoteClient
    replyBox <- newEmptyTMVarIO
    publish promptClient topic [withReplyCallback (atomically . putTMVar replyBox), withPayload "HELLO"]
    reply <- atomically $ readTMVar replyBox
    case reply of
      Nothing   -> expectationFailure "Expected a reply but got Nothing"
      Just msg' -> payload msg' `shouldBe` Just "WORLD"
    close remoteClient
    close promptClient
  clientSystemTest "bc32a4a4-91fb-4571-9c8e-5cb9c34de083" "request reports no responders" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    let topic = "REQ.NO_RESPONDERS"
    requester <- newClient [(natsHost, natsPort)] $
      withConnectName "no-responders-requester"
        : loggerOptions
    replyBox <- newEmptyTMVarIO
    (do
        publish requester topic [withReplyCallback (atomically . putTMVar replyBox), withPayload "HELLO"]
        flush requester
        replyResult <- timeout (5 * 1000000) (atomically (readTMVar replyBox))
        case replyResult of
          Nothing ->
            expectationFailure "request did not receive a no responders status"
          Just Nothing ->
            expectationFailure "request received an empty callback"
          Just (Just msg') -> do
            headers msg' `shouldBe` Just [("Status", "503"), ("Nats-Subject", topic)]
            payload msg' `shouldBe` Just "")
      `finally` close requester
