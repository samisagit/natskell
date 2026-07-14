{-# LANGUAGE OverloadedStrings #-}

module JetStream.MessageSpec (spec) where

import qualified Client.API              as Nats
import           Data.IORef
import           JetStream.Error         (JetStreamError (..))
import           JetStream.Message       (fetchMessages)
import           JetStream.Message.Types
import           JetStream.Options       (newJetStreamContext)
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
    , Nats.reset = \_ -> pure ()
    , Nats.close = \_ -> pure ()
    }
