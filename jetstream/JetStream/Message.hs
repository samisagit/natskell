module JetStream.Message
  ( messageAPI
  , fetchMessages
  , ackMessage
  , nakMessage
  , inProgressMessage
  , termMessage
  ) where

import qualified API                        as Nats
import           Control.Concurrent.MVar    (newEmptyMVar, takeMVar, tryPutMVar)
import           Control.Monad              (void)
import           JetStream.Error            (JetStreamError (JetStreamNoReply))
import           JetStream.Message.API
import           JetStream.Options          (JetStreamContext (..))
import qualified JetStream.Protocol.Subject as Subject
import           JetStream.Types            (ConsumerName, StreamName, Subject)
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
--
-- The current public client API creates a reply inbox that auto-unsubscribes
-- after one response for 'Nats.withReplyCallback'. To avoid requesting messages
-- that this client cannot observe, this function issues one safe @batch: 1@
-- request per requested message and stops on the first status response, local
-- timeout, or empty callback.
fetchMessages :: JetStreamContext -> StreamName -> ConsumerName -> PullRequest -> IO PullResponse
fetchMessages context stream consumer request
  | pullRequestBatch request <= 0 = pure (PullResponse [] Nothing)
  | otherwise = go (pullRequestBatch request) []
  where
    natsClient = contextClient context
    requestSubject = Subject.consumerNextSubject context stream consumer
    go 0 messages = pure (PullResponse (reverse messages) Nothing)
    go remaining messages = do
      result <- pullOne natsClient requestSubject request
      case result of
        PullResultMessage message ->
          go (remaining - 1) (message:messages)
        PullResultStatus status ->
          pure (PullResponse (reverse messages) (Just status))
        PullResultNone ->
          pure (PullResponse (reverse messages) Nothing)

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

pullOne :: Nats.Client -> Subject -> PullRequest -> IO PullResult
pullOne natsClient requestSubject request = do
  responseVar <- newEmptyMVar
  Nats.publish natsClient requestSubject
    [ Nats.withPayload (pullRequestPayload 1 request)
    , Nats.withReplyCallback (void . tryPutMVar responseVar)
    ]
  response <- timeout (responseTimeoutMicros request) (takeMVar responseVar)
  pure (classifyPullResult response)

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
