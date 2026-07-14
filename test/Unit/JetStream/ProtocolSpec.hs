{-# LANGUAGE OverloadedStrings #-}

module JetStream.ProtocolSpec (spec) where

import qualified Client.API                 as Nats
import           Data.Aeson                 (eitherDecodeStrict)
import           Data.Either                (isLeft)
import           JetStream.Error            (JetStreamError (..))
import           JetStream.Options
    ( JetStreamConfigError (..)
    , newJetStreamContext
    , requestTimeoutMicros
    , tryNewJetStreamContext
    , withDomain
    )
import           JetStream.Protocol.Headers (statusCode, statusDescription)
import           JetStream.Protocol.Request (decodeJetStreamResponse)
import           JetStream.Protocol.Subject
import           JetStream.Stream.Types     (StreamMessage (..))
import           JetStream.Types
    ( AccountInfo (..)
    , AccountTier (..)
    , AckPolicy (..)
    , DeliverPolicy
    , withRequestTimeout
    )
import           Test.Hspec

spec :: Spec
spec = do
  describe "JetStream subjects" $ do
    it "builds default API subjects" $ do
      streamInfoSubject testContext "ORDERS" `shouldBe` "$JS.API.STREAM.INFO.ORDERS"

    it "builds domain API subjects" $ do
      let ctx = newJetStreamContext fakeClient [withDomain "HUB"]
      consumerNextSubject ctx "ORDERS" "WORKER" `shouldBe` "$JS.HUB.API.CONSUMER.MSG.NEXT.ORDERS.WORKER"

    it "builds administrative API subjects" $ do
      accountInfoSubject testContext `shouldBe` "$JS.API.INFO"
      streamMessageGetSubject testContext "ORDERS" `shouldBe` "$JS.API.STREAM.MSG.GET.ORDERS"
      streamMessageDeleteSubject testContext "ORDERS" `shouldBe` "$JS.API.STREAM.MSG.DELETE.ORDERS"
      consumerPauseSubject testContext "ORDERS" "WORKER" `shouldBe` "$JS.API.CONSUMER.PAUSE.ORDERS.WORKER"
      consumerResetSubject testContext "ORDERS" "WORKER" `shouldBe` "$JS.API.CONSUMER.RESET.ORDERS.WORKER"

  describe "JetStream options" $ do
    it "rejects invalid construction options" $ do
      case tryNewJetStreamContext fakeClient [withDomain ""] of
        Left EmptyJetStreamDomain -> pure ()
        _                         -> expectationFailure "expected an empty-domain error"

    it "applies request options left to right" $ do
      requestTimeoutMicros testContext
        [withRequestTimeout 2, withRequestTimeout 0.25]
        `shouldBe` 250000

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

    it "decodes stored stream messages from base64 JSON fields" $ do
      case decodeJetStreamResponse storedMessageResponse of
        Right message -> do
          streamMessageSubject message `shouldBe` "ORDERS.created"
          streamMessageSequence message `shouldBe` 2
          streamMessagePayload message `shouldBe` Just "payload"
        other ->
          expectationFailure ("unexpected stored message decode: " ++ show (other :: Either JetStreamError StreamMessage))

    it "decodes account information" $ do
      case decodeJetStreamResponse accountInfoResponse of
        Right info -> do
          accountTierStreams (accountInfoTier info) `shouldBe` 1
          accountInfoDomain info `shouldBe` Just "hub"
          length (accountInfoTiers info) `shouldBe` 1
        other ->
          expectationFailure ("unexpected account info decode: " ++ show (other :: Either JetStreamError AccountInfo))

    it "rejects parameterised deliver policies without their companion fields" $ do
      (eitherDecodeStrict "\"by_start_sequence\"" :: Either String DeliverPolicy)
        `shouldSatisfy` isLeft

    it "preserves unknown wire enum values" $ do
      eitherDecodeStrict "\"future_ack_policy\""
        `shouldBe` Right (AckPolicyUnknown "future_ack_policy")
      (eitherDecodeStrict "\"by_start_time\"" :: Either String DeliverPolicy)
        `shouldSatisfy` isLeft

testContext =
  newJetStreamContext fakeClient []

storedMessageResponse =
  "{\"message\":{\"subject\":\"ORDERS.created\",\"seq\":2,\"data\":\"cGF5bG9hZA==\",\"time\":\"2024-01-01T00:00:00Z\"}}"

accountInfoResponse =
  "{\"memory\":10,\"storage\":20,\"reserved_memory\":0,\"reserved_storage\":0,\"streams\":1,\"consumers\":2,\"limits\":{\"max_memory\":-1,\"max_storage\":-1,\"max_streams\":-1,\"max_consumers\":-1,\"max_ack_pending\":-1,\"memory_max_stream_bytes\":-1,\"storage_max_stream_bytes\":-1,\"max_bytes_required\":false},\"domain\":\"hub\",\"api\":{\"level\":1,\"total\":3,\"errors\":0},\"tiers\":{\"R1\":{\"memory\":10,\"storage\":20,\"reserved_memory\":0,\"reserved_storage\":0,\"streams\":1,\"consumers\":2,\"limits\":{\"max_memory\":-1,\"max_storage\":-1,\"max_streams\":-1,\"max_consumers\":-1,\"max_ack_pending\":-1,\"memory_max_stream_bytes\":-1,\"storage_max_stream_bytes\":-1,\"max_bytes_required\":false}}}}"

fakeClient :: Nats.Client
fakeClient =
  Nats.Client
    { Nats.publish = \_ _ _ -> pure (Right ())
    , Nats.subscribe = \_ _ _ -> pure (Right (Nats.Subscription "0"))
    , Nats.subscribeOnce = \_ _ _ -> pure (Right (Nats.Subscription "0"))
    , Nats.request = \_ _ _ -> pure (Left Nats.NatsRequestTimedOut)
    , Nats.unsubscribe = \_ _ -> pure (Right ())
    , Nats.newInbox = pure "_INBOX.TEST"
    , Nats.ping = \_ -> pure (Right ())
    , Nats.flush = \_ -> pure (Right ())
    , Nats.reset = \_ -> pure ()
    , Nats.close = \_ -> pure ()
    }

fakeMsg :: Nats.Message
fakeMsg =
  Nats.Message
    { Nats.subject = "subject"
    , Nats.sid = "sid"
    , Nats.replyTo = Nothing
    , Nats.payload = ""
    , Nats.headers = Nothing
    }
