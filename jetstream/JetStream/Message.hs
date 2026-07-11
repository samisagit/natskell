module JetStream.Message
  ( messageAPI
  , fetchMessages
  , ackMessage
  , nakMessage
  , inProgressMessage
  , termMessage
  ) where

import qualified API                        as Nats
import           Control.Concurrent.STM
import           Control.Exception          (bracket)
import           JetStream.Error            (JetStreamError (JetStreamNoReply))
import           JetStream.Message.API
import           JetStream.Options          (JetStreamContext (..))
import qualified JetStream.Protocol.Subject as Subject
import           JetStream.Types            (ConsumerName, StreamName)
import           System.Timeout             (timeout)

-- | Build the message capability from the shared JetStream context.
messageAPI :: JetStreamContext -> MessageAPI
messageAPI context =
  MessageAPI
    { fetch = fetchMessages context
    , ack = ackMessage (contextClient context)
    , nak = nakMessage (contextClient context)
    , inProgress = inProgressMessage (contextClient context)
    , term = termMessage (contextClient context)
    }

-- | Fetch messages for a pull consumer.
fetchMessages :: JetStreamContext -> StreamName -> ConsumerName -> PullRequest -> IO PullResponse
fetchMessages context stream consumer request
  | pullRequestBatch request <= 0 = pure (PullResponse [] Nothing)
  | otherwise = do
      responseQueue <- newTQueueIO
      inbox <- Nats.newInbox natsClient
      bracket
        (Nats.subscribe natsClient inbox [] (atomically . writeTQueue responseQueue))
        (Nats.unsubscribe natsClient)
        (\_ -> do
            Nats.publish natsClient requestSubject
              [ Nats.withPayload (pullRequestPayload (pullRequestBatch request) request)
              , Nats.withReplyTo inbox
              ]
            collectResponses (responseTimeoutMicros request) responseQueue (pullRequestBatch request) [])
  where
    natsClient = contextClient context
    requestSubject = Subject.consumerNextSubject context stream consumer

ackMessage :: Nats.Client -> Message -> IO (Either JetStreamError ())
ackMessage natsClient = publishAck natsClient Ack

nakMessage :: Nats.Client -> Message -> IO (Either JetStreamError ())
nakMessage natsClient = publishAck natsClient Nak

inProgressMessage :: Nats.Client -> Message -> IO (Either JetStreamError ())
inProgressMessage natsClient = publishAck natsClient InProgress

termMessage :: Nats.Client -> Message -> IO (Either JetStreamError ())
termMessage natsClient = publishAck natsClient Term

data PullResult = PullResultMessage Message
                | PullResultStatus PullStatus
                | PullResultNone

collectResponses :: Int -> TQueue (Maybe Nats.MsgView) -> Int -> [Message] -> IO PullResponse
collectResponses _ _ 0 messages =
  pure (PullResponse (reverse messages) Nothing)
collectResponses waitMicros responseQueue remaining messages = do
  response <- timeout waitMicros (atomically (readTQueue responseQueue))
  case classifyPullResult response of
    PullResultMessage message ->
      collectResponses waitMicros responseQueue (remaining - 1) (message:messages)
    PullResultStatus status ->
      pure (PullResponse (reverse messages) (Just status))
    PullResultNone ->
      pure (PullResponse (reverse messages) Nothing)

classifyPullResult :: Maybe (Maybe Nats.MsgView) -> PullResult
classifyPullResult Nothing = PullResultNone
classifyPullResult (Just Nothing) = PullResultNone
classifyPullResult (Just (Just msgView)) =
  case messageStatus message of
    Nothing     -> PullResultMessage message
    Just status -> PullResultStatus status
  where
    message = fromMsgView msgView

fromMsgView :: Nats.MsgView -> Message
fromMsgView msgView =
  Message
    { messageSubject = Nats.subject msgView
    , messagePayload = Nats.payload msgView
    , messageHeaders = Nats.headers msgView
    , messageReplyTo = Nats.replyTo msgView
    , messageStatus = classifyStatusHeaders (Nats.headers msgView)
    }

publishAck :: Nats.Client -> AckVerb -> Message -> IO (Either JetStreamError ())
publishAck natsClient verb message =
  case messageReplyTo message of
    Nothing ->
      pure (Left JetStreamNoReply)
    Just reply ->
      Right <$> Nats.publish natsClient reply [Nats.withPayload (ackPayload verb)]

responseTimeoutMicros :: PullRequest -> Int
responseTimeoutMicros request
  | pullRequestNoWait request = max 1 (pullRequestTimeoutMicros request)
  | pullRequestTimeoutMicros request <= 0 = 1
  | otherwise = pullRequestTimeoutMicros request + 100000
