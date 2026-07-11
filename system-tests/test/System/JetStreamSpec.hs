{-# LANGUAGE OverloadedStrings #-}

module JetStreamSpec (spec) where

import qualified API                    as Nats
import           Client                 (newClient, withConnectName)
import           Control.Exception      (finally)
import qualified Data.ByteString        as BS
import           JetStream              (JetStream (..), newJetStream)
import qualified JetStream.Consumer.API as Consumer
import qualified JetStream.Message.API  as Message
import qualified JetStream.Publish.API  as Publish
import qualified JetStream.Stream.API   as Stream
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
  createdStream <- Stream.create (streams jetStream) streamConfig
  case createdStream of
    Right _  -> pure ()
    Left err -> expectationFailure ("stream create failed: " ++ show err)

  createdConsumer <- Consumer.createDurableConsumer (consumers jetStream) streamName durableName consumerConfig
  case createdConsumer of
    Right _  -> pure ()
    Left err -> expectationFailure ("durable consumer create failed: " ++ show err)

  published <- Publish.publish (publisher jetStream) subjectName payloadBody []
  case published of
    Right _  -> pure ()
    Left err -> expectationFailure ("publish failed: " ++ show err)

  firstFetch <- Message.fetch (messages jetStream) streamName durableName fetchOne
  case Message.pullResponseMessages firstFetch of
    [message] -> do
      Message.messageSubject message `shouldBe` subjectName
      Message.messagePayload message `shouldBe` Just payloadBody
      acknowledged <- Message.ack (messages jetStream) message
      acknowledged `shouldBe` Right ()
    messages' ->
      expectationFailure ("expected one JetStream message, got " ++ show (length messages'))
  Message.pullResponseStatus firstFetch `shouldBe` Nothing

  emptyFetch <- Message.fetch (messages jetStream) streamName durableName shortFetch
  Message.pullResponseMessages emptyFetch `shouldBe` []
  Message.pullResponseStatus emptyFetch `shouldBe` Just (Message.PullNoMessages (Just "No Messages"))

streamConfig :: Stream.StreamConfig
streamConfig =
  Stream.StreamConfig
    { Stream.streamConfigName = streamName
    , Stream.streamConfigSubjects = [subjectName]
    , Stream.streamConfigRetention = Just Stream.LimitsPolicy
    , Stream.streamConfigStorage = Just Stream.MemoryStorage
    , Stream.streamConfigDiscard = Nothing
    , Stream.streamConfigMaxMessages = Nothing
    , Stream.streamConfigMaxBytes = Nothing
    , Stream.streamConfigMaxAge = Nothing
    , Stream.streamConfigReplicas = Nothing
    , Stream.streamConfigDuplicateWindow = Nothing
    , Stream.streamConfigAllowDirect = Nothing
    }

consumerConfig :: Consumer.ConsumerConfig
consumerConfig =
  Consumer.emptyConsumerConfig
    { Consumer.consumerConfigDurableName = Just durableName
    , Consumer.consumerConfigName = Just durableName
    , Consumer.consumerConfigDeliverPolicy = Just Consumer.DeliverAll
    , Consumer.consumerConfigAckPolicy = Just Consumer.AckExplicit
    , Consumer.consumerConfigFilterSubject = Just subjectName
    }

fetchOne :: Message.PullRequest
fetchOne =
  Message.defaultPullRequest
    { Message.pullRequestBatch = 1
    , Message.pullRequestTimeoutMicros = 1000000
    }

shortFetch :: Message.PullRequest
shortFetch =
  Message.defaultPullRequest
    { Message.pullRequestBatch = 1
    , Message.pullRequestTimeoutMicros = 100000
    , Message.pullRequestNoWait = True
    }

streamName :: BS.ByteString
streamName = "NATSKELL_JS_SYSTEM"

durableName :: BS.ByteString
durableName = "NATSKELL_JS_DURABLE"

subjectName :: BS.ByteString
subjectName = "NATSKELL.JS.SYSTEM"

payloadBody :: BS.ByteString
payloadBody = "hello jetstream"
