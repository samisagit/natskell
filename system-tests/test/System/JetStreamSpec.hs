{-# LANGUAGE OverloadedStrings #-}

module JetStreamSpec (spec) where

import qualified API               as Nats
import           Client            (newClient, withConnectName)
import           Control.Exception (finally)
import qualified Data.ByteString   as BS
import           JetStream.API     (JetStream (..))
import qualified JetStream.API     as JetStream
import           JetStream.Client  (newJetStream)
import           NatsServerConfig
import           Test.Hspec
import           TestSupport

spec :: Spec
spec =
  systemTest $ do
    describe "jetstream" $ do
      around (withNatsContainerConfigNamed "33d3fe4e-d2b8-4e91-8f9e-58a817c5575f" jetStreamServerOptions) $ do
        it "acks a durable pull message and reports no messages" durablePullConsumerTest

jetStreamServerOptions :: NatsConfigOptions
jetStreamServerOptions =
  [ WithLogVerbosity NatsLogDebug
  , WithJetStream
  ]

durablePullConsumerTest :: Endpoints -> IO ()
durablePullConsumerTest (Endpoints natsHost natsPort) = do
  client <- newClient [(natsHost, natsPort)] $
    withConnectName "1bd1f928-3caf-4d10-98bc-355a25182a0e"
      : testLoggerOptions
  runJetStreamScenario client `finally` Nats.close client

runJetStreamScenario :: Nats.Client -> IO ()
runJetStreamScenario client = do
  let jetStream = newJetStream client []
  createdStream <- JetStream.create (streams jetStream) streamName [subjectName] streamOptions
  case createdStream of
    Right _  -> pure ()
    Left err -> expectationFailure ("stream create failed: " ++ show err)

  createdConsumer <- JetStream.createDurableConsumer (consumers jetStream) streamName durableName consumerOptions
  case createdConsumer of
    Right _  -> pure ()
    Left err -> expectationFailure ("durable consumer create failed: " ++ show err)

  mapM_ (publishPayload jetStream) payloadBodies

  firstFetch <- JetStream.fetch (messages jetStream) streamName durableName fetchBatch
  case JetStream.pullResponseMessages firstFetch of
    messages'@[_, _] -> do
      fmap JetStream.messageSubject messages' `shouldBe` [subjectName, subjectName]
      fmap JetStream.messagePayload messages' `shouldBe` fmap Just payloadBodies
      acknowledgements <- mapM (JetStream.ack (messages jetStream)) messages'
      acknowledgements `shouldBe` [Right (), Right ()]
    messages' ->
      expectationFailure ("expected two JetStream messages, got " ++ show (length messages'))
  JetStream.pullResponseStatus firstFetch `shouldBe` Nothing

  emptyFetch <- JetStream.fetch (messages jetStream) streamName durableName shortFetch
  JetStream.pullResponseMessages emptyFetch `shouldBe` []
  JetStream.pullResponseStatus emptyFetch `shouldBe` Just (JetStream.PullNoMessages (Just "No Messages"))

streamOptions :: [JetStream.StreamConfigOption]
streamOptions =
  [ JetStream.withRetention JetStream.LimitsPolicy
  , JetStream.withStorage JetStream.MemoryStorage
  ]

consumerOptions :: [JetStream.ConsumerConfigOption]
consumerOptions =
  [ JetStream.withConsumerName durableName
  , JetStream.withConsumerDeliverPolicy JetStream.DeliverAll
  , JetStream.withConsumerAckPolicy JetStream.AckExplicit
  , JetStream.withConsumerFilter (JetStream.ConsumerFilterSubject subjectName)
  ]

publishPayload :: JetStream -> BS.ByteString -> IO ()
publishPayload jetStream body = do
  published <- JetStream.publish (publisher jetStream) subjectName body []
  case published of
    Right _  -> pure ()
    Left err -> expectationFailure ("publish failed: " ++ show err)

fetchBatch :: [JetStream.FetchOption]
fetchBatch =
  [ JetStream.withFetchBatch 2
  , JetStream.withFetchWait (JetStream.FetchExpiresMicros 1000000)
  ]

shortFetch :: [JetStream.FetchOption]
shortFetch =
  [ JetStream.withFetchBatch 1
  , JetStream.withFetchWait (JetStream.FetchNoWaitMicros 100000)
  ]

streamName :: BS.ByteString
streamName = "NATSKELL_JS_SYSTEM"

durableName :: BS.ByteString
durableName = "NATSKELL_JS_DURABLE"

subjectName :: BS.ByteString
subjectName = "NATSKELL.JS.SYSTEM"

payloadBodies :: [BS.ByteString]
payloadBodies = ["hello jetstream one", "hello jetstream two"]
