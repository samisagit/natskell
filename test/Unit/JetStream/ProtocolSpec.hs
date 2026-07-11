{-# LANGUAGE OverloadedStrings #-}

module JetStream.ProtocolSpec (spec) where

import qualified API                        as Nats
import           JetStream.Error            (JetStreamError (..))
import           JetStream.Options          (newJetStreamContext, withDomain)
import           JetStream.Protocol.Headers (statusCode, statusDescription)
import           JetStream.Protocol.Request (decodeJetStreamResponse)
import           JetStream.Protocol.Subject
import           Test.Hspec

spec :: Spec
spec = do
  describe "JetStream subjects" $ do
    it "builds default API subjects" $ do
      streamInfoSubject testContext "ORDERS" `shouldBe` "$JS.API.STREAM.INFO.ORDERS"

    it "builds domain API subjects" $ do
      let ctx = newJetStreamContext fakeClient [withDomain "HUB"]
      consumerNextSubject ctx "ORDERS" "WORKER" `shouldBe` "$JS.HUB.API.CONSUMER.MSG.NEXT.ORDERS.WORKER"

  describe "JetStream status headers" $ do
    it "extracts status and description from normal headers" $ do
      let msg = fakeMsg { Nats.headers = Just [("Status", "404"), ("Description", "No Messages")] }
      statusCode msg `shouldBe` Just 404
      statusDescription msg `shouldBe` Just "No Messages"

  describe "JetStream response decoding" $ do
    it "decodes API error envelopes" $ do
      case decodeJetStreamResponse "{\"error\":{\"code\":404,\"err_code\":10059,\"description\":\"stream not found\"}}" of
        Left (JetStreamApiFailure err) ->
          show err `shouldContain` "stream not found"
        other ->
          expectationFailure ("unexpected decode result: " ++ show (other :: Either JetStreamError ()))

testContext =
  newJetStreamContext fakeClient []

fakeClient :: Nats.Client
fakeClient =
  Nats.Client
    { Nats.publish = \_ _ -> pure ()
    , Nats.subscribe = \_ _ _ -> pure "0"
    , Nats.request = \_ _ _ -> pure "0"
    , Nats.unsubscribe = \_ -> pure ()
    , Nats.newInbox = pure "_INBOX.TEST"
    , Nats.ping = id
    , Nats.flush = pure ()
    , Nats.reset = pure ()
    , Nats.close = pure ()
    }

fakeMsg :: Nats.MsgView
fakeMsg =
  Nats.MsgView
    { Nats.subject = "subject"
    , Nats.sid = "sid"
    , Nats.replyTo = Nothing
    , Nats.payload = Nothing
    , Nats.headers = Nothing
    }
