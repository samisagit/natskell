{-# LANGUAGE OverloadedStrings #-}

module ClientRepliesSpec (spec) where

import           API
    ( Client
    , NatsError (NatsNoResponders)
    , Subject
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
import           Control.Monad     (replicateM_, void)
import           Data.Maybe
import           Data.Word         (Word64)
import           GHC.Stats
import           System.Mem        (performMajorGC)
import           System.Timeout
import           Test.Hspec
import           TestSupport

spec :: Spec
spec = do
  clientSystemTest "5ddbe840-2f87-42fb-ae3a-4bb9c483dc78" "keeps request state bounded" $ \_ (Endpoints natsHost natsPort) -> do
    let topic = "REQ.SOAK"
    remoteClient <- newTestClient [(natsHost, natsPort)] [withConnectName "request-soak-responder"]
    promptClient <- newTestClient [(natsHost, natsPort)] [withConnectName "request-soak-requester"]
    (do
        void . subscribe remoteClient topic [] $ \msg ->
          void (publish remoteClient (fromJust (replyTo msg)) "WORLD" [])
        flush remoteClient [] `shouldReturn` Right ()
        requestMany 100 promptClient topic
        baselineBytes <- liveBytesAfterGC
        requestMany 10000 promptClient topic
        finalBytes <- liveBytesAfterGC
        finalBytes - baselineBytes `shouldSatisfy` (< 2 * 1024 * 1024))
      `finally` do
        close remoteClient []
        close promptClient []
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

requestMany :: Int -> Client -> Subject -> IO ()
requestMany total client topic =
  replicateM_ total $ do
    reply <- request client topic "HELLO" []
    fmap payload reply `shouldBe` Right "WORLD"

liveBytesAfterGC :: IO Word64
liveBytesAfterGC = do
  performMajorGC
  gcdetails_live_bytes . gc <$> getRTSStats
