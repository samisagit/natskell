{-# LANGUAGE OverloadedStrings #-}

-- This deliberately depends only on the public library. The declarations are
-- representative downstream calls; evaluating them is unnecessary because
-- compiling this executable is the compatibility check.
module Main (main) where

import qualified API                      as Nats
import qualified Client
import qualified JetStream.API            as JetStream
import qualified JetStream.API.Consumer   as Consumer
import qualified JetStream.API.Management as Management
import qualified JetStream.API.Message    as JetStreamMessage
import qualified JetStream.API.Publish    as JetStreamPublish
import qualified JetStream.API.Stream     as Stream
import qualified JetStream.Client         as JetStreamClient

connect :: IO (Either Client.ConnectError Nats.Client)
connect =
  Client.connect
    [configuredServer]
    [ Client.withConnectName "public-api-compile-test"
    , Client.withConnectionAttempts 1
    , Client.withMessageLimit (1024 * 1024)
    , Client.withPendingDeliveryLimits 65536 (64 * 1024 * 1024)
    , Client.withErrorHandler (const (pure ()))
    , Client.withServerErrorHandler (const (pure ()))
    , Client.withConnectionEventHandler inspectConnectionEvent
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

coreOperations :: Nats.Client -> IO ()
coreOperations client = do
  _ <- Nats.publish
    client
    "events.created"
    "payload"
    [Nats.withHeaders [("content-type", "text/plain")]]
  subscription <- Nats.subscribe
    client
    "events.*"
    [Nats.withQueueGroup "workers"]
    observeMessage
  case subscription of
    Left _       -> pure ()
    Right handle -> do
      _ <- Nats.unsubscribe client handle []
      pure ()
  _ <- Nats.request
    client
    "service.echo"
    "request"
    [Nats.withRequestTimeout 1]
  _ <- Nats.ping client [Nats.withPingTimeout 1]
  _ <- Nats.flush client [Nats.withFlushTimeout 1]
  state <- Nats.connectionState client
  inspectConnectionState state
  Nats.close client []

inspectConnectionState :: Nats.ConnectionState -> IO ()
inspectConnectionState state =
  case state of
    Nats.ConnectionConnecting   -> pure ()
    Nats.ConnectionConnected    -> pure ()
    Nats.ConnectionReconnecting -> pure ()
    Nats.ConnectionClosing _    -> pure ()
    Nats.ConnectionClosed _     -> pure ()

inspectServerError :: Client.ServerError -> IO ()
inspectServerError serverError = do
  Client.serverErrorReason serverError `seq` pure ()
  case Client.serverErrorKind serverError of
    Client.ServerErrorAuthentication -> pure ()
    Client.ServerErrorPermission     -> pure ()
    Client.ServerErrorProtocol       -> pure ()
    Client.ServerErrorResourceLimit  -> pure ()
    Client.ServerErrorConnection     -> pure ()
    Client.ServerErrorUnknown        -> pure ()

inspectConnectionEvent :: Client.ConnectionEvent -> IO ()
inspectConnectionEvent event =
  case event of
    Client.ConnectionEventDisconnected -> pure ()
    Client.ConnectionEventReconnected  -> pure ()
    Client.ConnectionEventClosed _     -> pure ()

observeMessage :: Nats.Message -> IO ()
observeMessage message = do
  let _ = Nats.subject message
      _ = Nats.payload message
      _ = Nats.replyTo message
      _ = Nats.headers message
  pure ()

jetStreamContext
  :: Nats.Client
  -> Either JetStreamClient.JetStreamConfigError JetStream.JetStream
jetStreamContext client =
  JetStreamClient.newJetStream
    client
    [ JetStreamClient.withDomain "hub"
    , JetStreamClient.withRequestTimeoutMicros 1000000
    ]

jetStreamOperations :: JetStream.JetStream -> IO ()
jetStreamOperations jetStream = do
  _ <- Management.accountInfo
    (JetStream.management jetStream)
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
  _ <- JetStreamMessage.ackSync messageAPI message []
  _ <- JetStreamMessage.nak messageAPI message []
  case JetStreamMessage.nakDelay 1 of
    Nothing -> pure ()
    Just delay -> do
      _ <- JetStreamMessage.nakWithDelay messageAPI message delay []
      pure ()
  _ <- JetStreamMessage.termSync messageAPI message []
  pure ()

inspectJetStreamApiError :: JetStream.JetStreamApiError -> IO ()
inspectJetStreamApiError err = do
  JetStream.apiErrorCode err `seq` pure ()
  JetStream.apiErrorCodeDetail err `seq` pure ()
  JetStream.apiErrorDescription err `seq` pure ()

main :: IO ()
main =
  connect
    `seq` serverBuilders
    `seq` coreOperations
    `seq` inspectServerError
    `seq` jetStreamContext
    `seq` jetStreamOperations
    `seq` messageOperations
    `seq` inspectJetStreamApiError
    `seq` pure ()
