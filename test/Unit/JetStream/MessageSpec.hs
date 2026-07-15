{-# LANGUAGE OverloadedStrings #-}

module JetStream.MessageSpec (spec) where

import qualified Client.API              as Nats
import           Data.Int                (Int64)
import           Data.IORef
import           Data.Ratio              ((%))
import           JetStream.Error         (JetStreamError (..))
import           JetStream.Message       (fetchMessages, messageAPI)
import           JetStream.Message.Types
import           JetStream.Options       (newJetStreamContext)
import           JetStream.Types         (withRequestTimeout)
import           Publish                 (defaultPublishConfig)
import           Publish.Config          (publishReplyTo)
import           Test.Hspec
import           Types.Msg               (Payload, SID, Subject)

spec :: Spec
spec = do
  describe "status classification" $ do
    it "classifies no-message status headers" $ do
      classifyStatusHeaders (Just [("Status", "404"), ("Description", "No Messages")])
        `shouldBe` Just (PullNoMessages (Just "No Messages"))

    it "classifies request timeout status headers case-insensitively" $ do
      classifyStatusHeaders (Just [("status", "408"), ("description", "Request Timeout")])
        `shouldBe` Just (PullRequestTimeout (Just "Request Timeout"))

    it "preserves unrecognised status details" $ do
      classifyStatusHeaders (Just [("Status", "503"), ("Description", "Service Unavailable")])
        `shouldBe` Just (PullStatusError "503" (Just "Service Unavailable"))

    it "ignores normal message headers" $ do
      classifyStatusHeaders (Just [("Nats-Stream", "ORDERS")])
        `shouldBe` Nothing

  describe "ack payloads" $ do
    it "encodes ack verbs" $ do
      ackPayload Ack `shouldBe` "+ACK"
      nakPayload `shouldBe` "-NAK"
      inProgressPayload `shouldBe` "+WPI"
      termPayload `shouldBe` "+TERM"

    it "encodes delayed NAKs as exact integer nanoseconds" $ do
      fmap nakDelayPayload (nakDelay 1.234567891234)
        `shouldBe` Just "-NAK {\"delay\":1234567891}"

    it "rejects delays shorter than one nanosecond" $ do
      nakDelay 0 `shouldBe` Nothing
      nakDelay 0.000000000999 `shouldBe` Nothing
      nakDelay (-1) `shouldBe` Nothing

    it "accepts the exact signed 64-bit nanosecond boundaries" $ do
      let oneNanosecond = fromRational (1 % 1000000000)
          maxNanoseconds = toInteger (maxBound :: Int64)
          maxDelay = fromRational (maxNanoseconds % 1000000000)
          overflowDelay = fromRational ((maxNanoseconds + 1) % 1000000000)
      fmap nakDelayPayload (nakDelay oneNanosecond)
        `shouldBe` Just "-NAK {\"delay\":1}"
      fmap nakDelayPayload (nakDelay maxDelay)
        `shouldBe` Just "-NAK {\"delay\":9223372036854775807}"
      nakDelay overflowDelay `shouldBe` Nothing

  describe "message dispositions" $ do
    it "keeps empty-option acknowledgements asynchronous" $ do
      publishes <- newIORef []
      requests <- newIORef []
      let api = messageAPI
            (newJetStreamContext (ackFakeClient publishes requests ackReply) [])
            (error "unused consumer API")

      ack api ackMessage [] `shouldReturn` Right ()
      readIORef publishes `shouldReturn` [(ackSubject, "+ACK")]
      readIORef requests `shouldReturn` []

    it "uses request-reply when acknowledgement request options are supplied" $ do
      publishes <- newIORef []
      requests <- newIORef []
      let api = messageAPI
            (newJetStreamContext (ackFakeClient publishes requests ackReply) [])
            (error "unused consumer API")

      ack api ackMessage [withRequestTimeout 0.25] `shouldReturn` Right ()
      readIORef publishes `shouldReturn` []
      readIORef requests `shouldReturn` [(ackSubject, "+ACK")]

    it "always confirms ackSync and termSync" $ do
      publishes <- newIORef []
      requests <- newIORef []
      let api = messageAPI
            (newJetStreamContext (ackFakeClient publishes requests ackReply) [])
            (error "unused consumer API")

      ackSync api ackMessage [] `shouldReturn` Right ()
      termSync api ackMessage [] `shouldReturn` Right ()
      readIORef publishes `shouldReturn` []
      readIORef requests `shouldReturn`
        [ (ackSubject, "+ACK")
        , (ackSubject, "+TERM")
        ]

    it "sends delayed NAKs with the selected confirmation mode" $ do
      publishes <- newIORef []
      requests <- newIORef []
      let api = messageAPI
            (newJetStreamContext (ackFakeClient publishes requests ackReply) [])
            (error "unused consumer API")
          Just delay = nakDelay 2.5

      nakWithDelay api ackMessage delay [] `shouldReturn` Right ()
      nakWithDelay api ackMessage delay [withRequestTimeout 0.25]
        `shouldReturn` Right ()
      readIORef publishes `shouldReturn`
        [(ackSubject, "-NAK {\"delay\":2500000000}")]
      readIORef requests `shouldReturn`
        [(ackSubject, "-NAK {\"delay\":2500000000}")]

    it "maps confirmed acknowledgement timeouts" $ do
      publishes <- newIORef []
      requests <- newIORef []
      let api = messageAPI
            (newJetStreamContext
              (ackFakeClient publishes requests (Left Nats.NatsRequestTimedOut))
              [])
            (error "unused consumer API")

      ackSync api ackMessage [] `shouldReturn` Left JetStreamTimeout

    it "maps confirmed acknowledgement status errors" $ do
      publishes <- newIORef []
      requests <- newIORef []
      let response = Right (ackReplyMessage "" (Just [("Status", "409"), ("Description", "consumer deleted")]))
          api = messageAPI
            (newJetStreamContext (ackFakeClient publishes requests response) [])
            (error "unused consumer API")

      termSync api ackMessage []
        `shouldReturn` Left (JetStreamStatusError 409 (Just "consumer deleted"))

    it "accepts confirmed acknowledgement reply payloads" $ do
      publishes <- newIORef []
      requests <- newIORef []
      let response = Right (ackReplyMessage "server metadata" Nothing)
          api = messageAPI
            (newJetStreamContext (ackFakeClient publishes requests response) [])
            (error "unused consumer API")

      ackSync api ackMessage [] `shouldReturn` Right ()

  describe "message metadata" $ do
    it "parses v1 JetStream ack reply subjects" $ do
      let metadata = messageMetadata $
            Message "ORDERS.created" "payload" Nothing
              (Just "$JS.ACK.ORDERS.WORKER.3.10.2.123000000000.4")
              Nothing
      fmap messageMetadataStream metadata `shouldBe` Just "ORDERS"
      fmap messageMetadataConsumer metadata `shouldBe` Just "WORKER"
      fmap messageMetadataNumDelivered metadata `shouldBe` Just 3
      fmap messageMetadataStreamSequence metadata `shouldBe` Just 10
      fmap messageMetadataConsumerSequence metadata `shouldBe` Just 2
      fmap messageMetadataTimestamp metadata `shouldBe` Just (read "1970-01-01 00:02:03 UTC")
      fmap messageMetadataNumPending metadata `shouldBe` Just 4
      fmap messageMetadataDomain metadata `shouldBe` Just Nothing

    it "parses v2 JetStream ack reply subjects with domains" $ do
      let metadata = messageMetadata $
            Message "ORDERS.created" "payload" Nothing
              (Just "$JS.ACK.HUB.ACCOUNT.ORDERS.WORKER.1.11.3.124000000000.0")
              Nothing
      fmap messageMetadataDomain metadata `shouldBe` Just (Just "HUB")
      fmap messageMetadataStreamSequence metadata `shouldBe` Just 11

    it "rejects non-JetStream reply subjects" $ do
      messageMetadata (Message "ORDERS.created" "" Nothing (Just "_INBOX.reply") Nothing)
        `shouldBe` Nothing

  describe "pull request payloads" $ do
    it "encodes one-message requests with an expires timeout" $ do
      pullRequestPayload 1 defaultPullRequest
        `shouldBe` "{\"batch\":1,\"expires\":1000000000}"

    it "encodes no-wait requests without expires" $ do
      let request = pullRequest [withFetchWait (FetchNoWaitMicros 1000000)]
      pullRequestPayload 1 request
        `shouldBe` "{\"batch\":1,\"no_wait\":true}"

  describe "pull fetch" $ do
    it "uses one explicit reply inbox for a batch request" $ do
      publishCalls <- newIORef []
      unsubscribeCalls <- newIORef []
      callbackRef <- newIORef Nothing
      let client = fakeClient publishCalls unsubscribeCalls callbackRef
          request = pullRequest [withFetchBatch 2]
      result <- fetchMessages (newJetStreamContext client []) "ORDERS" "WORKER" request []

      publishCalls' <- readIORef publishCalls
      publishCalls' `shouldBe`
        [ ( "$JS.API.CONSUMER.MSG.NEXT.ORDERS.WORKER"
          , "{\"batch\":2,\"expires\":1000000000}"
          , Just "_INBOX.batch"
          )
        ]
      unsubscribeCalls' <- readIORef unsubscribeCalls
      unsubscribeCalls' `shouldBe` ["sid-1"]
      case result of
        Left err ->
          expectationFailure ("fetch failed: " ++ show err)
        Right response -> do
          fmap messagePayload (pullResponseMessages response) `shouldBe` ["one", "two"]
          pullResponseStatus response `shouldBe` Nothing

    it "returns a JetStream timeout when no pull response arrives" $ do
      publishCalls <- newIORef []
      unsubscribeCalls <- newIORef []
      let client = silentFakeClient publishCalls unsubscribeCalls
          request = pullRequest [withFetchWait (FetchNoWaitMicros 1000)]
      result <- fetchMessages (newJetStreamContext client []) "ORDERS" "WORKER" request []

      result `shouldBe` Left JetStreamTimeout
      publishCalls' <- readIORef publishCalls
      publishCalls' `shouldBe`
        [ ( "$JS.API.CONSUMER.MSG.NEXT.ORDERS.WORKER"
          , "{\"batch\":1,\"no_wait\":true}"
          , Just "_INBOX.timeout"
          )
        ]
      unsubscribeCalls' <- readIORef unsubscribeCalls
      unsubscribeCalls' `shouldBe` ["sid-timeout"]

fakeClient
  :: IORef [(Subject, Payload, Maybe Subject)]
  -> IORef [SID]
  -> IORef (Maybe (Nats.MsgView -> IO ()))
  -> Nats.Client
fakeClient publishCalls unsubscribeCalls callbackRef =
  Nats.Client
    { Nats.publish = \subject body options -> do
        let replyTo' = publishReplyTo (foldl (flip ($)) defaultPublishConfig options)
        modifyIORef' publishCalls (++ [(subject, body, replyTo')])
        callback <- readIORef callbackRef
        case callback of
          Nothing ->
            pure ()
          Just deliver -> do
            deliver (fakeMsg "one")
            deliver (fakeMsg "two")
        pure (Right ())
    , Nats.subscribe = \_ _ callback -> do
        writeIORef callbackRef (Just callback)
        pure (Right (Nats.Subscription "sid-1"))
    , Nats.subscribeOnce = \_ _ _ -> pure (Right (Nats.Subscription "sid-once"))
    , Nats.request = \_ _ _ -> pure (Left Nats.NatsRequestTimedOut)
    , Nats.unsubscribe = \(Nats.Subscription sid) _ -> do
        modifyIORef' unsubscribeCalls (++ [sid])
        pure (Right ())
    , Nats.newInbox = pure "_INBOX.batch"
    , Nats.ping = \_ -> pure (Right ())
    , Nats.flush = \_ -> pure (Right ())
    , Nats.connectionState = pure Nats.ConnectionConnected
    , Nats.reset = \_ -> pure ()
    , Nats.close = \_ -> pure ()
    }

fakeMsg :: Payload -> Nats.Message
fakeMsg body =
  Nats.Message
    { Nats.subject = "ORDERS.created"
    , Nats.sid = "sid-1"
    , Nats.replyTo = Just "$JS.ACK.ORDERS.WORKER.1.1.1"
    , Nats.payload = body
    , Nats.headers = Nothing
    }

silentFakeClient
  :: IORef [(Subject, Payload, Maybe Subject)]
  -> IORef [SID]
  -> Nats.Client
silentFakeClient publishCalls unsubscribeCalls =
  Nats.Client
    { Nats.publish = \subject body options -> do
        let replyTo' = publishReplyTo (foldl (flip ($)) defaultPublishConfig options)
        modifyIORef' publishCalls (++ [(subject, body, replyTo')])
        pure (Right ())
    , Nats.subscribe = \_ _ _ ->
        pure (Right (Nats.Subscription "sid-timeout"))
    , Nats.subscribeOnce = \_ _ _ -> pure (Right (Nats.Subscription "sid-once"))
    , Nats.request = \_ _ _ -> pure (Left Nats.NatsRequestTimedOut)
    , Nats.unsubscribe = \(Nats.Subscription sid) _ -> do
        modifyIORef' unsubscribeCalls (++ [sid])
        pure (Right ())
    , Nats.newInbox = pure "_INBOX.timeout"
    , Nats.ping = \_ -> pure (Right ())
    , Nats.flush = \_ -> pure (Right ())
    , Nats.connectionState = pure Nats.ConnectionConnected
    , Nats.reset = \_ -> pure ()
    , Nats.close = \_ -> pure ()
    }

ackSubject :: Subject
ackSubject = "$JS.ACK.ORDERS.WORKER.1.1.1"

ackMessage :: Message
ackMessage = Message "ORDERS.created" "payload" Nothing (Just ackSubject) Nothing

ackReply :: Either Nats.NatsError Nats.MsgView
ackReply = Right (ackReplyMessage "" Nothing)

ackReplyMessage :: Payload -> Maybe Nats.Headers -> Nats.Message
ackReplyMessage body responseHeaders =
  Nats.Message
    { Nats.subject = "_INBOX.ack"
    , Nats.sid = "sid-ack"
    , Nats.replyTo = Nothing
    , Nats.payload = body
    , Nats.headers = responseHeaders
    }

ackFakeClient
  :: IORef [(Subject, Payload)]
  -> IORef [(Subject, Payload)]
  -> Either Nats.NatsError Nats.MsgView
  -> Nats.Client
ackFakeClient publishCalls requestCalls response =
  Nats.Client
    { Nats.publish = \subject body _ -> do
        modifyIORef' publishCalls (++ [(subject, body)])
        pure (Right ())
    , Nats.subscribe = \_ _ _ -> pure (Right (Nats.Subscription "sid-ack"))
    , Nats.subscribeOnce = \_ _ _ -> pure (Right (Nats.Subscription "sid-ack-once"))
    , Nats.request = \subject body _ -> do
        modifyIORef' requestCalls (++ [(subject, body)])
        pure response
    , Nats.unsubscribe = \_ _ -> pure (Right ())
    , Nats.newInbox = pure "_INBOX.ack"
    , Nats.ping = \_ -> pure (Right ())
    , Nats.flush = \_ -> pure (Right ())
    , Nats.connectionState = pure Nats.ConnectionConnected
    , Nats.reset = \_ -> pure ()
    , Nats.close = \_ -> pure ()
    }
