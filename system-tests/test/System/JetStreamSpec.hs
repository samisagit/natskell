{-# LANGUAGE OverloadedStrings #-}

module JetStreamSpec (spec) where

import qualified API                   as Nats
import           Client                (newClient, withConnectName)
import           Control.Concurrent
    ( newEmptyMVar
    , takeMVar
    , threadDelay
    , tryPutMVar
    )
import           Control.Exception     (finally)
import           Control.Monad         (forM_, unless, void, when)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.IORef            (atomicModifyIORef', newIORef, readIORef)
import           Data.List             (sort)
import           Data.Time.Clock       (addUTCTime, getCurrentTime)
import           JetStream.API         (JetStream (..))
import qualified JetStream.API         as JetStream
import           JetStream.Client      (newJetStream)
import           NatsServerConfig
import           System.Timeout        (timeout)
import           Test.Hspec
import           TestSupport

spec :: Spec
spec =
  systemTest $ do
    describe "jetstream" $ do
      jetStreamSystemTest "33d3fe4e-d2b8-4e91-8f9e-58a817c5575f"
        "acks a durable pull message and reports no messages"
        durablePullConsumerTest
      jetStreamSystemTest "a617d120-8846-48b0-a76d-b6a5975ee1c8"
        "deduplicates publishes by message id"
        duplicatePublishTest
      jetStreamSystemTest "2f78c7d3-fae8-408f-957e-51c22a442e9d"
        "enforces discard policy at stream message limits"
        discardPolicyTest
      jetStreamSystemTest "13f2340f-c606-40e3-98fe-66b0d06f2310"
        "removes acknowledged work queue messages"
        workQueueRetentionTest
      jetStreamSystemTest "1ff65118-b330-4b06-8592-8d331c0d8f6d"
        "redelivers after NAK"
        nakRedeliveryTest
      jetStreamSystemTest "227ca150-9ecf-4541-a627-a3b45ab3b778"
        "does not redeliver after TERM"
        termStopsRedeliveryTest
      jetStreamSystemTest "73b6d241-518d-4d2d-91b6-69138355246a"
        "redelivers after AckWait expires"
        ackWaitRedeliveryTest
      jetStreamSystemTest "375c712f-66f4-45bf-8505-9d756d0806e7"
        "honours DeliverLast and DeliverNew policies"
        deliverPolicyTest
      jetStreamSystemTest "c223d908-198c-44f5-94f8-251d85f78e55"
        "filters consumers by one or many subjects"
        consumerFilterTest
      jetStreamSystemTest "2c4c9ad8-e97e-4e18-9df9-edaa7397e93e"
        "purges only matching subjects"
        filteredPurgeTest
      jetStreamSystemTest "08b58a6b-c4fb-4bf4-ae3c-d536803dc199"
        "filters stream list and names responses"
        streamListFilterTest
      jetStreamSystemTest "8ac57555-3d68-4f07-8e45-28d0805bd107"
        "returns API failures after deleted resources are queried"
        deleteLifecycleTest
      jetStreamSystemTest "b06c0695-cfc6-49ef-81ed-a2279292c103"
        "performs stream and account administration"
        streamAdministrationTest
      jetStreamSystemTest "6b81aed3-829b-455c-8149-7997f29fefac"
        "performs consumer administration"
        consumerAdministrationTest
      jetStreamSystemTest "b8634868-d1da-4550-aa0a-2177e0f50c37"
        "delivers push consumer messages"
        pushConsumerTest
      jetStreamSystemTest "d9f102d7-22b9-4103-b5c1-6c3b8e6d0cd7"
        "fetches ordered consumer messages after reset"
        orderedConsumerTest
      jetStreamSystemTest "ff173d87-7377-4ff5-a6c8-83574508b591"
        "stops push consumer subscriptions gracefully"
        pushConsumerGracefulShutdownTest
      jetStreamSystemTest "a4249340-a096-469b-b1fb-17056515846f"
        "stops ordered consumers gracefully"
        orderedConsumerGracefulShutdownTest

jetStreamServerOptions :: NatsConfigOptions
jetStreamServerOptions =
  [ WithLogVerbosity NatsLogDebug
  , WithJetStream
  ]

jetStreamSystemTest :: String -> String -> (JetStream -> IO ()) -> Spec
jetStreamSystemTest testId label scenario =
  around (withNatsContainerConfigNamed testId jetStreamServerOptions) .
    it label $ \endpoints ->
      withJetStreamClient testId endpoints scenario

withJetStreamClient :: String -> Endpoints -> (JetStream -> IO ()) -> IO ()
withJetStreamClient testId (Endpoints natsHost natsPort) scenario = do
  client <- newClient [(natsHost, natsPort)] $
    withConnectName (BC.pack testId)
      : testLoggerOptions
  scenario (newJetStream client []) `finally` Nats.close client

durablePullConsumerTest :: JetStream -> IO ()
durablePullConsumerTest jetStream = do
  createStreamOrFail jetStream streamName [subjectName] streamOptions
  createDurableConsumerOrFail jetStream streamName durableName consumerOptions

  mapM_ (publishPayload jetStream subjectName) payloadBodies

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
  expectNoMessages emptyFetch

duplicatePublishTest :: JetStream -> IO ()
duplicatePublishTest jetStream = do
  let stream = "NATSKELL_JS_DEDUPE"
      subject = "NATSKELL.JS.DEDUPE"
  createStreamOrFail jetStream stream [subject] $
    streamOptions ++ [JetStream.withDuplicateWindow 60]
  firstAck <- publishPayloadWith jetStream subject "same-id payload" [JetStream.withMsgId "dedupe-1"]
  secondAck <- publishPayloadWith jetStream subject "same-id payload" [JetStream.withMsgId "dedupe-1"]
  JetStream.publishAckDuplicate firstAck `shouldSatisfy` (/= Just True)
  JetStream.publishAckDuplicate secondAck `shouldBe` Just True
  info <- streamInfoOrFail jetStream stream
  JetStream.streamStateMessages (JetStream.streamInfoState info) `shouldBe` 1

discardPolicyTest :: JetStream -> IO ()
discardPolicyTest jetStream = do
  let oldStream = "NATSKELL_JS_DISCARD_OLD"
      oldSubject = "NATSKELL.JS.DISCARD.OLD"
      newStream = "NATSKELL_JS_DISCARD_NEW"
      newSubject = "NATSKELL.JS.DISCARD.NEW"
  createStreamOrFail jetStream oldStream [oldSubject] $
    streamOptions ++ [JetStream.withMaxMessages 1, JetStream.withDiscard JetStream.DiscardOld]
  publishPayload jetStream oldSubject "old"
  publishPayload jetStream oldSubject "new"
  createDurableConsumerOrFail jetStream oldStream "DISCARD_OLD" $
    pullConsumerOptions "DISCARD_OLD" []
  oldMessage <- fetchOnePayload jetStream oldStream "DISCARD_OLD"
  oldMessage `shouldBe` Just "new"

  createStreamOrFail jetStream newStream [newSubject] $
    streamOptions ++ [JetStream.withMaxMessages 1, JetStream.withDiscard JetStream.DiscardNew]
  publishPayload jetStream newSubject "kept"
  failed <- JetStream.publish (publisher jetStream) newSubject "rejected" []
  expectJetStreamFailure "discard new publish" failed
  info <- streamInfoOrFail jetStream newStream
  JetStream.streamStateMessages (JetStream.streamInfoState info) `shouldBe` 1

workQueueRetentionTest :: JetStream -> IO ()
workQueueRetentionTest jetStream = do
  let stream = "NATSKELL_JS_WORK_QUEUE"
      subject = "NATSKELL.JS.WORK"
      durable = "WORKER"
  createStreamOrFail jetStream stream [subject]
    [ JetStream.withRetention JetStream.WorkQueuePolicy
    , JetStream.withStorage JetStream.MemoryStorage
    ]
  createDurableConsumerOrFail jetStream stream durable $
    pullConsumerOptions durable []
  publishPayload jetStream subject "work"
  message <- fetchOneMessage jetStream stream durable
  ackOrFail jetStream message
  eventually "work queue message to be removed" $ do
    info <- streamInfoOrFail jetStream stream
    pure (JetStream.streamStateMessages (JetStream.streamInfoState info) == 0)

nakRedeliveryTest :: JetStream -> IO ()
nakRedeliveryTest jetStream = do
  let stream = "NATSKELL_JS_NAK"
      subject = "NATSKELL.JS.NAK"
      durable = "NAK_WORKER"
  createStreamOrFail jetStream stream [subject] streamOptions
  createDurableConsumerOrFail jetStream stream durable $
    pullConsumerOptions durable []
  publishPayload jetStream subject "redeliver"
  firstMessage <- fetchOneMessage jetStream stream durable
  JetStream.messagePayload firstMessage `shouldBe` Just "redeliver"
  JetStream.nak (messages jetStream) firstMessage `shouldReturn` Right ()
  secondMessage <- fetchOneMessage jetStream stream durable
  JetStream.messagePayload secondMessage `shouldBe` Just "redeliver"
  ackOrFail jetStream secondMessage

termStopsRedeliveryTest :: JetStream -> IO ()
termStopsRedeliveryTest jetStream = do
  let stream = "NATSKELL_JS_TERM"
      subject = "NATSKELL.JS.TERM"
      durable = "TERM_WORKER"
  createStreamOrFail jetStream stream [subject] streamOptions
  createDurableConsumerOrFail jetStream stream durable $
    pullConsumerOptions durable []
  publishPayload jetStream subject "stop"
  message <- fetchOneMessage jetStream stream durable
  JetStream.term (messages jetStream) message `shouldReturn` Right ()
  empty <- JetStream.fetch (messages jetStream) stream durable shortFetch
  JetStream.pullResponseMessages empty `shouldBe` []
  expectNoMessages empty

ackWaitRedeliveryTest :: JetStream -> IO ()
ackWaitRedeliveryTest jetStream = do
  let stream = "NATSKELL_JS_ACK_WAIT"
      subject = "NATSKELL.JS.ACK_WAIT"
      durable = "ACK_WAIT_WORKER"
  createStreamOrFail jetStream stream [subject] streamOptions
  createDurableConsumerOrFail jetStream stream durable $
    pullConsumerOptions durable
      [ JetStream.withConsumerAckWait 0.2
      , JetStream.withConsumerMaxDeliver 2
      ]
  publishPayload jetStream subject "timeout"
  firstMessage <- fetchOneMessage jetStream stream durable
  JetStream.messagePayload firstMessage `shouldBe` Just "timeout"
  threadDelay 500000
  secondMessage <- fetchOneMessage jetStream stream durable
  JetStream.messagePayload secondMessage `shouldBe` Just "timeout"
  ackOrFail jetStream secondMessage

deliverPolicyTest :: JetStream -> IO ()
deliverPolicyTest jetStream = do
  let stream = "NATSKELL_JS_DELIVER"
      subject = "NATSKELL.JS.DELIVER"
  createStreamOrFail jetStream stream [subject] streamOptions
  publishPayload jetStream subject "old-1"
  publishPayload jetStream subject "old-2"

  createDurableConsumerOrFail jetStream stream "DELIVER_LAST" $
    pullConsumerOptions "DELIVER_LAST" [JetStream.withConsumerDeliverPolicy JetStream.DeliverLast]
  latest <- fetchOnePayload jetStream stream "DELIVER_LAST"
  latest `shouldBe` Just "old-2"

  createDurableConsumerOrFail jetStream stream "DELIVER_NEW" $
    pullConsumerOptions "DELIVER_NEW" [JetStream.withConsumerDeliverPolicy JetStream.DeliverNew]
  empty <- JetStream.fetch (messages jetStream) stream "DELIVER_NEW" shortFetch
  JetStream.pullResponseMessages empty `shouldBe` []
  expectNoMessages empty
  publishPayload jetStream subject "new"
  newest <- fetchOnePayload jetStream stream "DELIVER_NEW"
  newest `shouldBe` Just "new"

consumerFilterTest :: JetStream -> IO ()
consumerFilterTest jetStream = do
  let stream = "NATSKELL_JS_FILTER"
      prefix = "NATSKELL.JS.FILTER"
      subjectA = prefix <> ".A"
      subjectB = prefix <> ".B"
      subjectC = prefix <> ".C"
  createStreamOrFail jetStream stream [prefix <> ".*"] streamOptions
  createDurableConsumerOrFail jetStream stream "FILTER_ONE" $
    pullConsumerOptions "FILTER_ONE"
      [JetStream.withConsumerFilter (JetStream.ConsumerFilterSubject subjectA)]
  createDurableConsumerOrFail jetStream stream "FILTER_MANY" $
    pullConsumerOptions "FILTER_MANY"
      [JetStream.withConsumerFilter (JetStream.ConsumerFilterSubjects [subjectA, subjectC])]
  publishPayload jetStream subjectA "a"
  publishPayload jetStream subjectB "b"
  publishPayload jetStream subjectC "c"
  one <- fetchPayloads jetStream stream "FILTER_ONE" 1
  one `shouldBe` [Just "a"]
  many <- fetchPayloads jetStream stream "FILTER_MANY" 2
  many `shouldBe` [Just "a", Just "c"]

filteredPurgeTest :: JetStream -> IO ()
filteredPurgeTest jetStream = do
  let stream = "NATSKELL_JS_PURGE"
      prefix = "NATSKELL.JS.PURGE"
      subjectA = prefix <> ".A"
      subjectB = prefix <> ".B"
      durable = "PURGE_READER"
  createStreamOrFail jetStream stream [prefix <> ".*"] streamOptions
  publishPayload jetStream subjectA "a-1"
  publishPayload jetStream subjectA "a-2"
  publishPayload jetStream subjectB "b-1"
  purged <- expectRight "purge stream" $
    JetStream.purge (streams jetStream) stream [JetStream.withPurgeSubject subjectA]
  JetStream.purgeStreamSuccess purged `shouldBe` True
  JetStream.purgeStreamPurged purged `shouldBe` 2
  info <- streamInfoOrFail jetStream stream
  JetStream.streamStateMessages (JetStream.streamInfoState info) `shouldBe` 1
  createDurableConsumerOrFail jetStream stream durable $
    pullConsumerOptions durable []
  remaining <- fetchOnePayload jetStream stream durable
  remaining `shouldBe` Just "b-1"

streamListFilterTest :: JetStream -> IO ()
streamListFilterTest jetStream = do
  let streamA = "NATSKELL_JS_LIST_A"
      streamB = "NATSKELL_JS_LIST_B"
      subjectA = "NATSKELL.JS.LIST.A"
      subjectB = "NATSKELL.JS.LIST.B"
  createStreamOrFail jetStream streamA [subjectA] streamOptions
  createStreamOrFail jetStream streamB [subjectB] streamOptions
  matchingNames <- expectRight "stream names" $
    JetStream.names (streams jetStream) [JetStream.withStreamListSubject subjectA]
  JetStream.streamNamesStreams matchingNames `shouldBe` [streamA]
  matchingList <- expectRight "stream list" $
    JetStream.list (streams jetStream) [JetStream.withStreamListSubject subjectA]
  fmap (JetStream.streamConfigName . JetStream.streamInfoConfig) (JetStream.streamListStreams matchingList)
    `shouldBe` [streamA]
  pagedNames <- expectRight "stream names with offset" $
    JetStream.names (streams jetStream) [JetStream.withStreamListOffset 1]
  sort (JetStream.streamNamesStreams pagedNames) `shouldBe` [streamB]

deleteLifecycleTest :: JetStream -> IO ()
deleteLifecycleTest jetStream = do
  let stream = "NATSKELL_JS_DELETE"
      subject = "NATSKELL.JS.DELETE"
      durable = "DELETE_ME"
  createStreamOrFail jetStream stream [subject] streamOptions
  createDurableConsumerOrFail jetStream stream durable $
    pullConsumerOptions durable []
  deletedConsumer <- expectRight "delete consumer" $
    JetStream.deleteConsumer (consumers jetStream) stream durable
  JetStream.deleteConsumerSuccess deletedConsumer `shouldBe` True
  consumerInfoResult <- JetStream.consumerInfo (consumers jetStream) stream durable
  expectJetStreamFailure "consumer info after delete" consumerInfoResult
  deletedStream <- expectRight "delete stream" $
    JetStream.delete (streams jetStream) stream
  JetStream.deleteStreamSuccess deletedStream `shouldBe` True
  streamInfoResult <- JetStream.info (streams jetStream) stream
  expectJetStreamFailure "stream info after delete" streamInfoResult

streamAdministrationTest :: JetStream -> IO ()
streamAdministrationTest jetStream = do
  account <- expectRight "account info" $
    JetStream.accountInfo jetStream
  JetStream.accountTierStreams (JetStream.accountInfoTier account) `shouldSatisfy` (>= 0)

  let stream = "NATSKELL_JS_ADMIN_STREAM"
      subjectA = "NATSKELL.JS.ADMIN.A"
      subjectB = "NATSKELL.JS.ADMIN.B"
  created <- expectRight "create or update stream" $
    JetStream.createOrUpdate
      (streams jetStream)
      stream
      [subjectA, subjectB]
      (streamOptions ++ [JetStream.withMaxMessages 10])
  JetStream.streamConfigName (JetStream.streamInfoConfig created) `shouldBe` stream
  updated <- expectRight "update stream" $
    JetStream.update
      (streams jetStream)
      stream
      [subjectA, subjectB]
      (streamOptions ++ [JetStream.withMaxMessages 5])
  JetStream.streamConfigMaxMessages (JetStream.streamInfoConfig updated) `shouldBe` 5

  firstAck <- publishPayloadWith jetStream subjectA "admin-a" []
  secondAck <- publishPayloadWith jetStream subjectB "admin-b" []
  firstMessage <- expectRight "get stream message by sequence" $
    JetStream.getMessage
      (streams jetStream)
      stream
      (JetStream.StreamMessageBySequence (fromIntegral (JetStream.publishAckSequence firstAck)))
  JetStream.streamMessagePayload firstMessage `shouldBe` Just "admin-a"
  lastForSubject <- expectRight "get last stream message by subject" $
    JetStream.getMessage
      (streams jetStream)
      stream
      (JetStream.LastStreamMessageForSubject subjectB)
  JetStream.streamMessagePayload lastForSubject `shouldBe` Just "admin-b"

  deleted <- expectRight "delete stream message" $
    JetStream.deleteMessage
      (streams jetStream)
      stream
      (fromIntegral (JetStream.publishAckSequence firstAck))
      JetStream.DeleteMessage
  JetStream.deleteStreamMessageSuccess deleted `shouldBe` True
  missing <- JetStream.getMessage
    (streams jetStream)
    stream
    (JetStream.StreamMessageBySequence (fromIntegral (JetStream.publishAckSequence firstAck)))
  expectJetStreamFailure "get deleted stream message" missing

  secureDeleted <- expectRight "secure delete stream message" $
    JetStream.deleteMessage
      (streams jetStream)
      stream
      (fromIntegral (JetStream.publishAckSequence secondAck))
      JetStream.SecureDeleteMessage
  JetStream.deleteStreamMessageSuccess secureDeleted `shouldBe` True

consumerAdministrationTest :: JetStream -> IO ()
consumerAdministrationTest jetStream = do
  let stream = "NATSKELL_JS_ADMIN_CONSUMER"
      subject = "NATSKELL.JS.ADMIN.CONSUMER"
      durable = "ADMIN_CONSUMER"
  createStreamOrFail jetStream stream [subject] streamOptions
  mapM_
    (publishPayload jetStream subject)
    ["msg-1", "msg-2", "msg-3", "msg-4"]

  created <- expectRight "create or update durable consumer" $
    JetStream.putConsumer
      (consumers jetStream)
      stream
      JetStream.ConsumerCreateOrUpdate
      (JetStream.DurableConsumer durable)
      JetStream.PullConsumer
      (pullConsumerOptions durable [JetStream.withConsumerDescription "v1"])
  JetStream.consumerInfoName created `shouldBe` durable
  updated <- expectRight "update durable consumer" $
    JetStream.putConsumer
      (consumers jetStream)
      stream
      JetStream.ConsumerUpdate
      (JetStream.DurableConsumer durable)
      JetStream.PullConsumer
      (pullConsumerOptions durable
        [ JetStream.withConsumerDescription "v2"
        , JetStream.withConsumerMaxAckPending 64
        ])
  JetStream.consumerConfigDescription (JetStream.consumerInfoConfig updated) `shouldBe` Just "v2"
  JetStream.consumerConfigMaxAckPending (JetStream.consumerInfoConfig updated) `shouldBe` Just 64

  pauseUntil <- addUTCTime 2 <$> getCurrentTime
  paused <- expectRight "pause consumer" $
    JetStream.pauseConsumer (consumers jetStream) stream durable pauseUntil
  JetStream.consumerPausePaused paused `shouldBe` True
  resumed <- expectRight "resume consumer" $
    JetStream.resumeConsumer (consumers jetStream) stream durable
  JetStream.consumerPausePaused resumed `shouldBe` False

  firstBatch <- JetStream.fetch (messages jetStream) stream durable
    [ JetStream.withFetchBatch 2
    , JetStream.withFetchWait (JetStream.FetchExpiresMicros 1000000)
    ]
  length (JetStream.pullResponseMessages firstBatch) `shouldBe` 2
  resetToSecond <- expectRight "reset consumer to sequence" $
    JetStream.resetConsumer
      (consumers jetStream)
      stream
      durable
      [JetStream.withConsumerResetSequence 2]
  JetStream.consumerResetResponseSequence resetToSecond `shouldBe` 2
  afterReset <- fetchOnePayload jetStream stream durable
  afterReset `shouldBe` Just "msg-2"

pushConsumerTest :: JetStream -> IO ()
pushConsumerTest jetStream = do
  let stream = "NATSKELL_JS_PUSH"
      subject = "NATSKELL.JS.PUSH"
      consumer = "PUSH_CONSUMER"
      deliverSubject = "NATSKELL.JS.PUSH.DELIVER"
      payloads = ["push-1", "push-2", "push-3"]
  createStreamOrFail jetStream stream [subject] streamOptions
  created <- expectRight "push consumer create" $
    JetStream.putConsumer
      (consumers jetStream)
      stream
      JetStream.ConsumerCreate
      (JetStream.NamedConsumer consumer)
      (JetStream.PushConsumer deliverSubject)
      [ JetStream.withConsumerAckPolicy JetStream.AckExplicit
      , JetStream.withConsumerDeliverPolicy JetStream.DeliverAll
      ]
  JetStream.consumerConfigDeliverSubject (JetStream.consumerInfoConfig created)
    `shouldBe` Just deliverSubject

  receivedRef <- newIORef []
  done <- newEmptyMVar
  subscription <- JetStream.consumePush (messages jetStream) deliverSubject [] $ \message -> do
    count <- atomicModifyIORef' receivedRef $ \received ->
      let next =
            if length received < length payloads
              then received ++ [JetStream.messagePayload message]
              else received
      in (next, length next)
    void (JetStream.ack (messages jetStream) message)
    when (count >= length payloads) $
      void (tryPutMVar done ())

  mapM_ (publishPayload jetStream subject) payloads
  result <- timeout 5000000 (takeMVar done)
  JetStream.stopPushSubscription subscription
  result `shouldBe` Just ()
  received <- readIORef receivedRef
  received `shouldBe` fmap Just payloads

orderedConsumerTest :: JetStream -> IO ()
orderedConsumerTest jetStream = do
  let stream = "NATSKELL_JS_ORDERED"
      subject = "NATSKELL.JS.ORDERED"
      payloads = ["ordered-1", "ordered-2", "ordered-3", "ordered-4", "ordered-5"]
  createStreamOrFail jetStream stream [subject] streamOptions
  mapM_ (publishPayload jetStream subject) payloads
  ordered <- expectRight "ordered consumer create" $
    JetStream.createOrderedConsumer
      (messages jetStream)
      stream
      [JetStream.withOrderedConsumerNamePrefix "ORDERED_SYSTEM"]

  firstBatch <- expectRight "ordered first fetch" $
    JetStream.fetchOrdered ordered
      [ JetStream.withFetchBatch 2
      , JetStream.withFetchWait (JetStream.FetchExpiresMicros 1000000)
      ]
  fmap JetStream.messagePayload (JetStream.pullResponseMessages firstBatch)
    `shouldBe` fmap Just (take 2 payloads)

  current <- expectRight "ordered consumer info" $
    JetStream.orderedConsumerInfo ordered
  deleted <- expectRight "delete ordered consumer" $
    JetStream.deleteConsumer
      (consumers jetStream)
      stream
      (JetStream.consumerInfoName current)
  JetStream.deleteConsumerSuccess deleted `shouldBe` True

  secondBatch <- expectRight "ordered second fetch" $
    JetStream.fetchOrdered ordered
      [ JetStream.withFetchBatch 3
      , JetStream.withFetchWait (JetStream.FetchExpiresMicros 1000000)
      ]
  fmap JetStream.messagePayload (JetStream.pullResponseMessages secondBatch)
    `shouldBe` fmap Just (drop 2 payloads)
  JetStream.stopOrderedConsumer ordered

pushConsumerGracefulShutdownTest :: JetStream -> IO ()
pushConsumerGracefulShutdownTest jetStream = do
  let stream = "NATSKELL_JS_PUSH_STOP"
      subject = "NATSKELL.JS.PUSH.STOP"
      consumer = "PUSH_STOP_CONSUMER"
      deliverSubject = "NATSKELL.JS.PUSH.STOP.DELIVER"
  createStreamOrFail jetStream stream [subject] streamOptions
  void . expectRight "push stop consumer create" $
    JetStream.putConsumer
      (consumers jetStream)
      stream
      JetStream.ConsumerCreate
      (JetStream.NamedConsumer consumer)
      (JetStream.PushConsumer deliverSubject)
      [ JetStream.withConsumerAckPolicy JetStream.AckExplicit
      , JetStream.withConsumerDeliverPolicy JetStream.DeliverAll
      ]

  stoppedCallback <- newEmptyMVar
  subscription <- JetStream.consumePush (messages jetStream) deliverSubject [] $ \message ->
    void (tryPutMVar stoppedCallback message)
  stopResult <- timeout 1000000 (JetStream.stopPushSubscription subscription)
  stopResult `shouldBe` Just ()

  publishPayload jetStream subject "after-stop"
  stoppedDelivery <- timeout 1000000 (takeMVar stoppedCallback)
  stoppedDelivery `shouldBe` Nothing

  resumedCallback <- newEmptyMVar
  resumed <- JetStream.consumePush (messages jetStream) deliverSubject [] $ \message -> do
    ackOrFail jetStream message
    when (JetStream.messagePayload message == Just "after-resume") $
      void (tryPutMVar resumedCallback message)
  publishPayload jetStream subject "after-resume"
  resumedDelivery <- timeout 5000000 (takeMVar resumedCallback)
  JetStream.stopPushSubscription resumed
  fmap JetStream.messagePayload resumedDelivery `shouldBe` Just (Just "after-resume")

orderedConsumerGracefulShutdownTest :: JetStream -> IO ()
orderedConsumerGracefulShutdownTest jetStream = do
  let stream = "NATSKELL_JS_ORDERED_STOP"
      subject = "NATSKELL.JS.ORDERED.STOP"
  createStreamOrFail jetStream stream [subject] streamOptions
  ordered <- expectRight "ordered stop consumer create" $
    JetStream.createOrderedConsumer
      (messages jetStream)
      stream
      [JetStream.withOrderedConsumerNamePrefix "ORDERED_STOP"]
  current <- expectRight "ordered stop consumer info" $
    JetStream.orderedConsumerInfo ordered

  stopResult <- timeout 1000000 (JetStream.stopOrderedConsumer ordered)
  stopResult `shouldBe` Just ()

  consumerInfoAfterStop <- JetStream.consumerInfo
    (consumers jetStream)
    stream
    (JetStream.consumerInfoName current)
  expectJetStreamFailure "ordered consumer info after stop" consumerInfoAfterStop
  orderedInfoAfterStop <- JetStream.orderedConsumerInfo ordered
  expectJetStreamFailure "ordered handle info after stop" orderedInfoAfterStop
  fetchAfterStop <- JetStream.fetchOrdered ordered
    [ JetStream.withFetchBatch 1
    , JetStream.withFetchWait (JetStream.FetchNoWaitMicros 100000)
    ]
  fetchAfterStop `shouldBe` Left JetStream.JetStreamNoReply

streamOptions :: [JetStream.StreamConfigOption]
streamOptions =
  [ JetStream.withRetention JetStream.LimitsPolicy
  , JetStream.withStorage JetStream.MemoryStorage
  ]

consumerOptions :: [JetStream.ConsumerConfigOption]
consumerOptions =
  pullConsumerOptions durableName
    [JetStream.withConsumerFilter (JetStream.ConsumerFilterSubject subjectName)]

pullConsumerOptions :: BS.ByteString -> [JetStream.ConsumerConfigOption] -> [JetStream.ConsumerConfigOption]
pullConsumerOptions _durable extraOptions =
  extraOptions ++
  [ JetStream.withConsumerDeliverPolicy JetStream.DeliverAll
  , JetStream.withConsumerAckPolicy JetStream.AckExplicit
  ]

createStreamOrFail
  :: JetStream
  -> BS.ByteString
  -> [BS.ByteString]
  -> [JetStream.StreamConfigOption]
  -> IO JetStream.StreamInfo
createStreamOrFail jetStream name subjects options =
  expectRight ("stream create " ++ BC.unpack name) $
    JetStream.create (streams jetStream) name subjects options

createDurableConsumerOrFail
  :: JetStream
  -> BS.ByteString
  -> BS.ByteString
  -> [JetStream.ConsumerConfigOption]
  -> IO JetStream.ConsumerInfo
createDurableConsumerOrFail jetStream stream durable options =
  expectRight ("durable consumer create " ++ BC.unpack durable) $
    JetStream.putConsumer
      (consumers jetStream)
      stream
      JetStream.ConsumerCreate
      (JetStream.DurableConsumer durable)
      JetStream.PullConsumer
      options

streamInfoOrFail :: JetStream -> BS.ByteString -> IO JetStream.StreamInfo
streamInfoOrFail jetStream stream =
  expectRight ("stream info " ++ BC.unpack stream) $
    JetStream.info (streams jetStream) stream

publishPayload :: JetStream -> BS.ByteString -> BS.ByteString -> IO ()
publishPayload jetStream subject body =
  void (publishPayloadWith jetStream subject body [])

publishPayloadWith
  :: JetStream
  -> BS.ByteString
  -> BS.ByteString
  -> [JetStream.PublishOption]
  -> IO JetStream.PublishAck
publishPayloadWith jetStream subject body options =
  expectRight ("publish " ++ BC.unpack subject) $
    JetStream.publish (publisher jetStream) subject body options

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

fetchOnePayload :: JetStream -> BS.ByteString -> BS.ByteString -> IO (Maybe BS.ByteString)
fetchOnePayload jetStream stream durable =
  JetStream.messagePayload <$> fetchOneMessage jetStream stream durable

fetchOneMessage :: JetStream -> BS.ByteString -> BS.ByteString -> IO JetStream.Message
fetchOneMessage jetStream stream durable = do
  response <- JetStream.fetch (messages jetStream) stream durable
    [ JetStream.withFetchBatch 1
    , JetStream.withFetchWait (JetStream.FetchExpiresMicros 1000000)
    ]
  case JetStream.pullResponseMessages response of
    [message] ->
      pure message
    received ->
      expectationFailure ("expected one JetStream message, got " ++ show (length received)) >>
        fail "expected one JetStream message"

fetchPayloads :: JetStream -> BS.ByteString -> BS.ByteString -> Int -> IO [Maybe BS.ByteString]
fetchPayloads jetStream stream durable batch = do
  response <- JetStream.fetch (messages jetStream) stream durable
    [ JetStream.withFetchBatch batch
    , JetStream.withFetchWait (JetStream.FetchExpiresMicros 1000000)
    ]
  let received = JetStream.pullResponseMessages response
  unless (length received == batch) $
    expectationFailure ("expected " ++ show batch ++ " JetStream messages, got " ++ show (length received))
  forM_ received (ackOrFail jetStream)
  pure (fmap JetStream.messagePayload received)

ackOrFail :: JetStream -> JetStream.Message -> IO ()
ackOrFail jetStream message =
  JetStream.ack (messages jetStream) message `shouldReturn` Right ()

expectNoMessages :: JetStream.PullResponse -> IO ()
expectNoMessages response =
  case JetStream.pullResponseStatus response of
    Just (JetStream.PullNoMessages _) ->
      pure ()
    other ->
      expectationFailure ("expected no-messages status, got " ++ show other)

expectRight :: Show err => String -> IO (Either err value) -> IO value
expectRight label action = do
  result <- action
  case result of
    Right value ->
      pure value
    Left err ->
      expectationFailure (label ++ " failed: " ++ show err) >>
        fail (label ++ " failed")

expectJetStreamFailure :: Show value => String -> Either JetStream.JetStreamError value -> IO ()
expectJetStreamFailure label result =
  case result of
    Left (JetStream.JetStreamApiFailure _) ->
      pure ()
    Left other ->
      expectationFailure (label ++ " returned non-API failure: " ++ show other)
    Right value ->
      expectationFailure (label ++ " unexpectedly succeeded: " ++ show value)

eventually :: String -> IO Bool -> IO ()
eventually label condition =
  go (20 :: Int)
  where
    go 0 =
      expectationFailure ("timed out waiting for " ++ label)
    go attempts = do
      satisfied <- condition
      unless satisfied $ do
        threadDelay 100000
        go (attempts - 1)

streamName :: BS.ByteString
streamName = "NATSKELL_JS_SYSTEM"

durableName :: BS.ByteString
durableName = "NATSKELL_JS_DURABLE"

subjectName :: BS.ByteString
subjectName = "NATSKELL.JS.SYSTEM"

payloadBodies :: [BS.ByteString]
payloadBodies = ["hello jetstream one", "hello jetstream two"]
