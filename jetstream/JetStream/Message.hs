{-# LANGUAGE LambdaCase #-}

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
import           Data.Word                  (Word64)
import           JetStream.Consumer.API
    ( ConsumerAPI
    , consumerInfo
    , deleteConsumer
    , putConsumer
    )
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
    ( JetStreamError (JetStreamDecodeError, JetStreamNatsError, JetStreamNoReply, JetStreamTimeout)
    )
import           JetStream.Message.Types
import           JetStream.Options
    ( JetStreamContext (..)
    , requestTimeoutMicros
    )
import           JetStream.Protocol.Headers (statusError)
import           JetStream.Protocol.Request (requestMsg)
import qualified JetStream.Protocol.Subject as Subject
import           JetStream.Types
    ( AckPolicy (AckNone)
    , ConsumerName
    , DeliverPolicy (DeliverByStartSequence)
    , JetStreamRequestOption
    , Payload
    , StreamName
    , Subject
    )
import           System.Timeout             (timeout)

-- | Build the message capability from the shared JetStream context.
messageAPI :: JetStreamContext -> ConsumerAPI -> MessageAPI
messageAPI context consumerAPI =
  MessageAPI
    { fetch = \stream consumer options requestOptions ->
        fetchMessages context stream consumer (pullRequest options) requestOptions
    , consumePush = \deliverSubject options _ handler ->
        consumePushMessages context deliverSubject (pushConsumeConfig options) handler
    , createOrderedConsumer = \stream options requestOptions ->
        createOrderedConsumerHandle context consumerAPI stream (orderedConsumerConfig options) requestOptions
    , ack = sendDisposition context ConfirmWhenRequested (ackPayload Ack)
    , ackSync = sendDisposition context ConfirmAlways (ackPayload Ack)
    , nak = sendDisposition context ConfirmWhenRequested (ackPayload Nak)
    , nakWithDelay = \message delay ->
        sendDisposition context ConfirmWhenRequested (nakDelayPayload delay) message
    , inProgress = sendDisposition context ConfirmWhenRequested (ackPayload InProgress)
    , term = sendDisposition context ConfirmWhenRequested (ackPayload Term)
    , termSync = sendDisposition context ConfirmAlways (ackPayload Term)
    }

consumePushMessages
  :: JetStreamContext
  -> Subject
  -> PushConsumeConfig
  -> (Message -> IO ())
  -> IO (Either JetStreamError PushSubscription)
consumePushMessages context deliverSubject config handler = do
  subscription <- Nats.subscribe natsClient deliverSubject subscribeOptions $ \msgView ->
    let message = fromMsgView msgView
    in unless (isStatusMessage message) (handler message)
  pure $ case subscription of
    Left err -> Left (JetStreamNatsError err)
    Right handle -> Right PushSubscription
      { stopPushSubscription = mapNatsResult <$> Nats.unsubscribe natsClient handle []
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
                      , orderedStateNextSequence :: TVar (Maybe Word64)
                      , orderedStateCurrentName  :: TVar (Maybe ConsumerName)
                      , orderedStateStopped      :: TVar Bool
                      }

createOrderedConsumerHandle
  :: JetStreamContext
  -> ConsumerAPI
  -> StreamName
  -> OrderedConsumerConfig
  -> [JetStreamRequestOption]
  -> IO (Either JetStreamError OrderedConsumer)
createOrderedConsumerHandle context consumerAPI stream config requestOptions = do
  namePrefix <- orderedNamePrefix context config
  state <- OrderedState context consumerAPI stream config namePrefix
    <$> newTVarIO 0
    <*> newTVarIO Nothing
    <*> newTVarIO Nothing
    <*> newTVarIO False
  created <- resetOrderedConsumer state requestOptions
  pure $ do
    void created
    Right OrderedConsumer
      { orderedConsumerInfo = orderedInfo state
      , fetchOrdered = fetchOrderedMessages state
      , stopOrderedConsumer = stopOrdered state
      }

orderedInfo :: OrderedState -> [JetStreamRequestOption] -> IO (Either JetStreamError ConsumerInfo)
orderedInfo state requestOptions = do
  currentName <- readTVarIO (orderedStateCurrentName state)
  case currentName of
    Nothing ->
      pure (Left JetStreamNoReply)
    Just consumerName ->
      consumerInfo
        (orderedStateConsumers state)
        (orderedStateStream state)
        consumerName
        requestOptions

fetchOrderedMessages :: OrderedState -> [FetchOption] -> [JetStreamRequestOption] -> IO (Either JetStreamError PullResponse)
fetchOrderedMessages state options requestOptions = do
  resetResult <- resetOrderedConsumer state requestOptions
  case resetResult of
    Left err ->
      pure (Left err)
    Right info -> do
      responseResult <- fetchMessages
        (orderedStateContext state)
        (orderedStateStream state)
        (consumerInfoName info)
        (pullRequest options)
        requestOptions
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

resetOrderedConsumer :: OrderedState -> [JetStreamRequestOption] -> IO (Either JetStreamError ConsumerInfo)
resetOrderedConsumer state requestOptions = do
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
              requestOptions
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
        requestOptions
      case result of
        Left err ->
          pure (Left err)
        Right info -> do
          atomically $
            writeTVar (orderedStateCurrentName state) (Just consumerName)
          pure (Right info)

stopOrdered :: OrderedState -> [JetStreamRequestOption] -> IO (Either JetStreamError ())
stopOrdered state requestOptions = do
  atomically $
    writeTVar (orderedStateStopped state) True
  currentName <- readTVarIO (orderedStateCurrentName state)
  case currentName of
    Nothing ->
      pure (Right ())
    Just consumerName ->
      void <$>
        deleteConsumer
          (orderedStateConsumers state)
          (orderedStateStream state)
          consumerName
          requestOptions

orderedConsumerOptions
  :: OrderedConsumerConfig
  -> Maybe Word64
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
          DeliverByStartSequence sequenceNumber
    inactiveThreshold =
      fromMaybe 300 (orderedConsumerInactiveThreshold config)

orderedResponseNextSequence :: PullResponse -> Either JetStreamError (Maybe Word64)
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
fetchMessages :: JetStreamContext -> StreamName -> ConsumerName -> PullRequest -> [JetStreamRequestOption] -> IO (Either JetStreamError PullResponse)
fetchMessages context stream consumer request requestOptions
  | pullRequestBatch request <= 0 = pure (Right (PullResponse [] Nothing))
  | otherwise = do
      responseQueue <- newTQueueIO
      inbox <- Nats.newInbox natsClient
      bracket
        (Nats.subscribe natsClient inbox [] (atomically . writeTQueue responseQueue))
        (\case
            Left _       -> pure ()
            Right handle -> void (Nats.unsubscribe natsClient handle []))
        (\case
            Left err ->
              pure (Left (JetStreamNatsError err))
            Right _ -> do
              published <- Nats.publish natsClient requestSubject
                (pullRequestPayload (pullRequestBatch request) request)
                [Nats.withReplyTo inbox]
              case published of
                Left err ->
                  pure (Left (JetStreamNatsError err))
                Right () ->
                  collectResponses waitMicros responseQueue (pullRequestBatch request) [])
  where
    natsClient = contextClient context
    requestSubject = Subject.consumerNextSubject context stream consumer
    waitMicros = min
      (responseTimeoutMicros request)
      (requestTimeoutMicros context requestOptions)

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

collectResponses :: Int -> TQueue Nats.MsgView -> Int -> [Message] -> IO (Either JetStreamError PullResponse)
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

classifyPullResult :: Maybe Nats.MsgView -> PullResult
classifyPullResult Nothing = PullResultTimeout
classifyPullResult (Just msgView) =
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
      mapNatsResult <$> Nats.publish natsClient reply (ackPayload verb) []

data ConfirmationMode = ConfirmWhenRequested | ConfirmAlways

sendDisposition
  :: JetStreamContext
  -> ConfirmationMode
  -> Payload
  -> Message
  -> [JetStreamRequestOption]
  -> IO (Either JetStreamError ())
sendDisposition context confirmationMode body message requestOptions =
  case messageReplyTo message of
    Nothing ->
      pure (Left JetStreamNoReply)
    Just reply
      | confirmationRequired confirmationMode requestOptions ->
          confirmDisposition context reply body requestOptions
      | otherwise ->
          mapNatsResult <$> Nats.publish (contextClient context) reply body []

confirmDisposition
  :: JetStreamContext
  -> Subject
  -> Payload
  -> [JetStreamRequestOption]
  -> IO (Either JetStreamError ())
confirmDisposition context reply body requestOptions = do
  response <- requestMsg context reply body [] requestOptions
  pure $ do
    message <- response
    case statusError message of
      Just err ->
        Left err
      Nothing ->
        Right ()

confirmationRequired :: ConfirmationMode -> [JetStreamRequestOption] -> Bool
confirmationRequired ConfirmAlways _ = True
confirmationRequired ConfirmWhenRequested requestOptions = not (null requestOptions)

mapNatsResult :: Either Nats.NatsError () -> Either JetStreamError ()
mapNatsResult =
  either (Left . JetStreamNatsError) Right

responseTimeoutMicros :: PullRequest -> Int
responseTimeoutMicros request =
  case pullRequestWait request of
    FetchNoWaitMicros timeoutMicros ->
      max 1 timeoutMicros
    FetchExpiresMicros timeoutMicros
      | timeoutMicros <= 0 -> 1
      | otherwise -> timeoutMicros + 100000
