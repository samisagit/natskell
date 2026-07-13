module JetStream.Message
  ( messageAPI
  , fetchMessages
  , ackMessage
  , nakMessage
  , inProgressMessage
  , termMessage
  ) where

import qualified Client.API                 as Nats
import           Control.Concurrent.STM
import           Control.Exception          (bracket)
import           Control.Monad              (unless, void)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BC
import           Data.Char                  (isAlphaNum)
import           Data.Maybe                 (catMaybes, fromMaybe)
import           JetStream.Consumer.API     (ConsumerAPI (..))
import           JetStream.Consumer.Types
    ( ConsumerAction (ConsumerCreateOrUpdate)
    , ConsumerConfigOption
    , ConsumerInfo (consumerInfoName)
    , ConsumerKind (PullConsumer)
    , ConsumerTarget (NamedConsumer)
    , withConsumerAckPolicy
    , withConsumerDeliverPolicy
    , withConsumerFilter
    , withConsumerHeadersOnly
    , withConsumerInactiveThreshold
    , withConsumerMaxDeliver
    , withConsumerMaxWaiting
    , withConsumerMemoryStorage
    , withConsumerReplayPolicy
    , withConsumerReplicas
    )
import           JetStream.Error
    ( JetStreamError (JetStreamDecodeError, JetStreamNoReply, JetStreamTimeout)
    )
import           JetStream.Message.API      (MessageAPI (..))
import           JetStream.Message.Types
import           JetStream.Options          (JetStreamContext (..))
import qualified JetStream.Protocol.Subject as Subject
import           JetStream.Types
    ( AckPolicy (AckNone)
    , ConsumerName
    , DeliverPolicy (DeliverByStartSequence)
    , StreamName
    , Subject
    )
import           System.Timeout             (timeout)

-- | Build the message capability from the shared JetStream context.
messageAPI :: JetStreamContext -> ConsumerAPI -> MessageAPI
messageAPI context consumerAPI =
  MessageAPI
    { fetch = \stream consumer options ->
        fetchMessages context stream consumer (pullRequest options)
    , consumePush = \deliverSubject options handler ->
        consumePushMessages context deliverSubject (pushConsumeConfig options) handler
    , createOrderedConsumer = \stream options ->
        createOrderedConsumerHandle context consumerAPI stream (orderedConsumerConfig options)
    , ack = ackMessage (contextClient context)
    , nak = nakMessage (contextClient context)
    , inProgress = inProgressMessage (contextClient context)
    , term = termMessage (contextClient context)
    }

consumePushMessages
  :: JetStreamContext
  -> Subject
  -> PushConsumeConfig
  -> (Message -> IO ())
  -> IO PushSubscription
consumePushMessages context deliverSubject config handler = do
  sid <- Nats.subscribe natsClient deliverSubject subscribeOptions $ \msgView ->
    case fmap fromMsgView msgView of
      Nothing ->
        pure ()
      Just message ->
        unless (isStatusMessage message) (handler message)
  pure PushSubscription
    { stopPushSubscription = Nats.unsubscribe natsClient sid
    }
  where
    natsClient = contextClient context
    subscribeOptions =
      maybe [] (\queueGroup -> [Nats.withQueueGroup queueGroup]) (pushConsumeQueueGroup config)

data OrderedState = OrderedState
                      { orderedStateContext      :: JetStreamContext
                      , orderedStateConsumers    :: ConsumerAPI
                      , orderedStateStream       :: StreamName
                      , orderedStateConfig       :: OrderedConsumerConfig
                      , orderedStateNamePrefix   :: ConsumerName
                      , orderedStateSerial       :: TVar Int
                      , orderedStateNextSequence :: TVar (Maybe Integer)
                      , orderedStateCurrentName  :: TVar (Maybe ConsumerName)
                      , orderedStateStopped      :: TVar Bool
                      }

createOrderedConsumerHandle
  :: JetStreamContext
  -> ConsumerAPI
  -> StreamName
  -> OrderedConsumerConfig
  -> IO (Either JetStreamError OrderedConsumer)
createOrderedConsumerHandle context consumerAPI stream config = do
  namePrefix <- orderedNamePrefix context config
  state <- OrderedState context consumerAPI stream config namePrefix
    <$> newTVarIO 0
    <*> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newTVarIO False
  created <- resetOrderedConsumer state
  pure $ do
    void created
    Right OrderedConsumer
      { orderedConsumerInfo = orderedInfo state
      , fetchOrdered = fetchOrderedMessages state
      , stopOrderedConsumer = stopOrdered state
      }

orderedInfo :: OrderedState -> IO (Either JetStreamError ConsumerInfo)
orderedInfo state = do
  currentName <- readTVarIO (orderedStateCurrentName state)
  case currentName of
    Nothing ->
      pure (Left JetStreamNoReply)
    Just consumerName ->
      consumerInfo
        (orderedStateConsumers state)
        (orderedStateStream state)
        consumerName

fetchOrderedMessages :: OrderedState -> [FetchOption] -> IO (Either JetStreamError PullResponse)
fetchOrderedMessages state options = do
  resetResult <- resetOrderedConsumer state
  case resetResult of
    Left err ->
      pure (Left err)
    Right info -> do
      responseResult <- fetchMessages
        (orderedStateContext state)
        (orderedStateStream state)
        (consumerInfoName info)
        (pullRequest options)
      case responseResult of
        Left err ->
          pure (Left err)
        Right response ->
          case orderedResponseNextSequence response of
            Left err ->
              pure (Left err)
            Right Nothing ->
              pure (Right response)
            Right (Just nextSequence) -> do
              atomically $
                writeTVar (orderedStateNextSequence state) (Just nextSequence)
              pure (Right response)

resetOrderedConsumer :: OrderedState -> IO (Either JetStreamError ConsumerInfo)
resetOrderedConsumer state = do
  stopped <- readTVarIO (orderedStateStopped state)
  if stopped
    then pure (Left JetStreamNoReply)
    else do
      previous <- readTVarIO (orderedStateCurrentName state)
      case previous of
        Nothing ->
          pure ()
        Just consumerName ->
          void $
            deleteConsumer
              (orderedStateConsumers state)
              (orderedStateStream state)
              consumerName
      (serial, nextSequence) <- atomically $ do
        serial <- succ <$> readTVar (orderedStateSerial state)
        writeTVar (orderedStateSerial state) serial
        nextSequence <- readTVar (orderedStateNextSequence state)
        pure (serial, nextSequence)
      let consumerName = orderedConsumerName (orderedStateNamePrefix state) serial
          options = orderedConsumerOptions (orderedStateConfig state) nextSequence
      result <- putConsumer
        (orderedStateConsumers state)
        (orderedStateStream state)
        ConsumerCreateOrUpdate
        (NamedConsumer consumerName)
        PullConsumer
        options
      case result of
        Left err ->
          pure (Left err)
        Right info -> do
          atomically $
            writeTVar (orderedStateCurrentName state) (Just consumerName)
          pure (Right info)

stopOrdered :: OrderedState -> IO ()
stopOrdered state = do
  atomically $
    writeTVar (orderedStateStopped state) True
  currentName <- readTVarIO (orderedStateCurrentName state)
  case currentName of
    Nothing ->
      pure ()
    Just consumerName ->
      void $
        deleteConsumer
          (orderedStateConsumers state)
          (orderedStateStream state)
          consumerName

orderedConsumerOptions
  :: OrderedConsumerConfig
  -> Maybe Integer
  -> [ConsumerConfigOption]
orderedConsumerOptions config nextSequence =
  catMaybes
    [ Just (withConsumerAckPolicy AckNone)
    , Just (withConsumerDeliverPolicy deliverPolicy)
    , Just (withConsumerMaxDeliver (-1))
    , Just (withConsumerMaxWaiting 512)
    , Just (withConsumerInactiveThreshold inactiveThreshold)
    , Just (withConsumerReplicas 1)
    , Just (withConsumerMemoryStorage True)
    , withConsumerFilter <$> orderedConsumerFilter config
    , withConsumerReplayPolicy <$> orderedConsumerReplayPolicy config
    , withConsumerHeadersOnly <$> orderedConsumerHeadersOnly config
    ]
  where
    deliverPolicy =
      case nextSequence of
        Nothing ->
          orderedConsumerDeliverPolicy config
        Just sequenceNumber ->
          DeliverByStartSequence (fromInteger sequenceNumber)
    inactiveThreshold =
      fromMaybe 300 (orderedConsumerInactiveThreshold config)

orderedResponseNextSequence :: PullResponse -> Either JetStreamError (Maybe Integer)
orderedResponseNextSequence response =
  case reverse (pullResponseMessages response) of
    [] ->
      Right Nothing
    lastMessage:_ ->
      case messageMetadata lastMessage of
        Nothing ->
          Left (JetStreamDecodeError "missing JetStream message metadata")
        Just metadata ->
          Right (Just (messageMetadataStreamSequence metadata + 1))

orderedNamePrefix :: JetStreamContext -> OrderedConsumerConfig -> IO ConsumerName
orderedNamePrefix context config =
  case orderedConsumerNamePrefix config of
    Just namePrefix ->
      pure namePrefix
    Nothing -> do
      inbox <- Nats.newInbox (contextClient context)
      pure (BS.append (BC.pack "ORDERED_") (sanitizeConsumerName inbox))

orderedConsumerName :: ConsumerName -> Int -> ConsumerName
orderedConsumerName namePrefix serial =
  BS.concat [namePrefix, BC.pack "_", BC.pack (show serial)]

sanitizeConsumerName :: Subject -> ConsumerName
sanitizeConsumerName =
  BC.map replace
  where
    replace char
      | isAlphaNum char = char
      | otherwise = '_'

-- | Fetch messages for a pull consumer.
fetchMessages :: JetStreamContext -> StreamName -> ConsumerName -> PullRequest -> IO (Either JetStreamError PullResponse)
fetchMessages context stream consumer request
  | pullRequestBatch request <= 0 = pure (Right (PullResponse [] Nothing))
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
                | PullResultTimeout
                | PullResultClosed

collectResponses :: Int -> TQueue (Maybe Nats.MsgView) -> Int -> [Message] -> IO (Either JetStreamError PullResponse)
collectResponses _ _ 0 messages =
  pure (Right (PullResponse (reverse messages) Nothing))
collectResponses waitMicros responseQueue remaining messages = do
  response <- timeout waitMicros (atomically (readTQueue responseQueue))
  case classifyPullResult response of
    PullResultMessage message ->
      collectResponses waitMicros responseQueue (remaining - 1) (message:messages)
    PullResultStatus status ->
      pure (Right (PullResponse (reverse messages) (Just status)))
    PullResultTimeout ->
      pure (Left JetStreamTimeout)
    PullResultClosed ->
      pure (Left JetStreamNoReply)

classifyPullResult :: Maybe (Maybe Nats.MsgView) -> PullResult
classifyPullResult Nothing = PullResultTimeout
classifyPullResult (Just Nothing) = PullResultClosed
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
responseTimeoutMicros request =
  case pullRequestWait request of
    FetchNoWaitMicros timeoutMicros ->
      max 1 timeoutMicros
    FetchExpiresMicros timeoutMicros
      | timeoutMicros <= 0 -> 1
      | otherwise -> timeoutMicros + 100000
