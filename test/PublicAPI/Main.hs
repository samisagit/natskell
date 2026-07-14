{-# LANGUAGE OverloadedStrings #-}

-- This deliberately depends only on the public library. The declarations are
-- representative downstream calls; evaluating them is unnecessary because
-- compiling this executable is the compatibility check.
module Main (main) where

import qualified Nats
import qualified Nats.Client             as Client
import qualified Nats.JetStream          as JetStream
import qualified Nats.JetStream.Consumer as Consumer
import qualified Nats.JetStream.Error    as JetStreamError
import qualified Nats.JetStream.Message  as JetStreamMessage
import qualified Nats.JetStream.Publish  as JetStreamPublish
import qualified Nats.JetStream.Stream   as Stream
import qualified Nats.Message            as Message
import qualified Nats.Subscription       as Subscription

connect :: IO (Either Client.ConnectError Client.Client)
connect =
  Nats.connect
    [configuredServer]
    [ Client.withConnectName "public-api-compile-test"
    , Client.withConnectionAttempts 1
    ]

configuredServer :: Client.Server
configuredServer =
  case Client.server "127.0.0.1" 4222 of
    Left err       -> error (show err)
    Right endpoint -> endpoint

serverBuilders
  :: ( Either Client.ServerConfigError Client.Server
     , Either Client.ServerConfigError Client.Server
     )
serverBuilders =
  (Client.server "nats.example" 4222, Client.serverWithDefaultPort "nats.example")

coreOperations :: Client.Client -> IO ()
coreOperations client = do
  _ <- Client.publish
    client
    "events.created"
    "payload"
    [Client.withHeaders [("content-type", "text/plain")]]
  subscription <- Subscription.subscribe
    client
    "events.*"
    [Subscription.withQueueGroup "workers"]
    observeMessage
  case subscription of
    Left _       -> pure ()
    Right handle -> do
      _ <- Subscription.unsubscribe client handle []
      pure ()
  _ <- Client.request
    client
    "service.echo"
    "request"
    [Client.withRequestTimeout 1]
  _ <- Client.ping client []
  _ <- Client.flush client []
  Client.close client []

observeMessage :: Message.Message -> IO ()
observeMessage message = do
  let _ = Message.subject message
      _ = Message.payload message
      _ = Message.replyTo message
      _ = Message.headers message
  pure ()

jetStreamContext
  :: Client.Client
  -> Either JetStream.JetStreamConfigError JetStream.JetStream
jetStreamContext client =
  JetStream.newJetStream
    client
    [ JetStream.withDomain "hub"
    , JetStream.withRequestTimeoutMicros 1000000
    ]

jetStreamOperations :: JetStream.JetStream -> IO ()
jetStreamOperations jetStream = do
  _ <- JetStream.accountInfo
    jetStream
    [JetStream.withRequestTimeout 1]
  created <- Stream.createStream
    (JetStream.streams jetStream)
    "ORDERS"
    ["orders.>"]
    [ Stream.withStorage Stream.FileStorage
    , Stream.withMaxMessages 1000
    ]
    []
  case created of
    Left _       -> pure ()
    Right handle -> do
      _ <- Stream.getStreamInfo (JetStream.streams jetStream) handle []
      pure ()
  _ <- JetStreamPublish.publish
    (JetStream.publisher jetStream)
    "orders.created"
    "order"
    [JetStreamPublish.withMsgId "api-compile-test"]
    []
  consumer <- Consumer.putConsumerHandle
    (JetStream.consumers jetStream)
    "ORDERS"
    Consumer.ConsumerCreate
    (Consumer.DurableConsumer "processor")
    Consumer.PullConsumer
    [Consumer.withConsumerAckPolicy Consumer.AckExplicit]
    []
  case consumer of
    Left _       -> pure ()
    Right handle -> do
      _ <- Consumer.getConsumerInfo (JetStream.consumers jetStream) handle []
      pure ()
  _ <- JetStreamMessage.fetch
    (JetStream.messages jetStream)
    "ORDERS"
    "processor"
    [JetStreamMessage.withFetchBatch 10]
    []
  _ <- JetStreamMessage.consumePush
    (JetStream.messages jetStream)
    "deliver.orders"
    []
    []
    (const (pure ()))
  pure ()

messageOperations
  :: JetStream.MessageAPI
  -> JetStreamMessage.Message
  -> IO ()
messageOperations messageAPI message = do
  JetStreamMessage.messageMetadata message `seq` pure ()
  _ <- JetStreamMessage.ack messageAPI message []
  _ <- JetStreamMessage.nak messageAPI message []
  pure ()

inspectJetStreamApiError :: JetStreamError.JetStreamApiError -> IO ()
inspectJetStreamApiError err = do
  JetStreamError.apiErrorCode err `seq` pure ()
  JetStreamError.apiErrorCodeDetail err `seq` pure ()
  JetStreamError.apiErrorDescription err `seq` pure ()

main :: IO ()
main =
  connect
    `seq` serverBuilders
    `seq` coreOperations
    `seq` jetStreamContext
    `seq` jetStreamOperations
    `seq` messageOperations
    `seq` inspectJetStreamApiError
    `seq` pure ()
