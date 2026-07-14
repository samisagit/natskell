{-# LANGUAGE OverloadedStrings #-}

module ClientRepliesSpec (spec) where

import           API
    ( Client
    , NatsError (NatsNoResponders)
    , close
    , flush
    , payload
    , publish
    , replyTo
    , request
    , subscribe
    , withRequestTimeout
    )
import           Client
import           Control.Exception (finally)
import           Control.Monad     (void)
import           Data.Maybe
import           System.Timeout
import           Test.Hspec
import           TestSupport

spec :: Spec
spec = do
  clientSystemTest "79b1247b-10b6-4c1c-8d78-20a9e9f30cc0" "replies are routed correctly" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    let topic = "REQ.TOPIC"
    remoteClient <- newTestClient [(natsHost, natsPort)] $
      withConnectName "6eff2527-1ad5-4b0c-b4e5-4a52a7d17639"
        : loggerOptions
    promptClient <- newTestClient [(natsHost, natsPort)] $
      withConnectName "6eff2527-1ad5-4b0c-b4e5-4a52a7d17639"
        : loggerOptions
    void . subscribe remoteClient topic [] $ \msg ->
      void (publish remoteClient (fromJust (replyTo msg)) "WORLD" [])
    flush remoteClient [] `shouldReturn` Right ()
    reply <- request promptClient topic "HELLO" []
    fmap payload reply `shouldBe` Right "WORLD"
    close remoteClient []
    close promptClient []
  clientSystemTest "bc32a4a4-91fb-4571-9c8e-5cb9c34de083" "request reports no responders" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    let topic = "REQ.NO_RESPONDERS"
    requester <- newTestClient [(natsHost, natsPort)] $
      withConnectName "no-responders-requester"
        : loggerOptions
    (do
        replyResult <- timeout (5 * 1000000) $
          request requester topic "HELLO" [withRequestTimeout 2]
        case replyResult of
          Nothing ->
            expectationFailure "request did not receive a no responders status"
          Just (Left NatsNoResponders) ->
            pure ()
          Just other ->
            expectationFailure ("unexpected request result: " ++ show other))
      `finally` close requester []
