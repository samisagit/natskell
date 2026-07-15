{-# LANGUAGE OverloadedStrings #-}

module JetStream.ConsumerSpec (spec) where

import           Data.Aeson
    ( Value
    , eitherDecode
    , encode
    , object
    , (.=)
    )
import qualified Data.ByteString.Lazy     as LBS
import           Data.Int                 (Int64)
import           Data.Ratio               ((%))
import           JetStream.Consumer.Types
import           JetStream.Error
    ( JetStreamError (JetStreamDecodeError)
    )
import           Test.Hspec

spec :: Spec
spec = do
  describe "ConsumerConfig request JSON" $ do
    it "encodes only populated fields" $ do
      eitherDecode (encode durablePullConsumerConfigRequest) `shouldBe` Right durablePullConsumerConfigValue

    it "encodes push and ordered consumer fields" $ do
      eitherDecode (encode pushConsumerConfigRequest) `shouldBe` Right pushConsumerConfigValue

    it "lets consumer target and kind own mutually exclusive fields" $ do
      eitherDecode (encode targetKindConsumerConfigRequest)
        `shouldBe` Right targetKindConsumerConfigValue

    it "applies consumer options from left to right" $ do
      let request = consumerConfigRequest
            [ withConsumerDeliverPolicy DeliverAll
            , withConsumerDeliverPolicy DeliverLast
            ]
      eitherDecode (encode request)
        `shouldBe` Right (object ["deliver_policy" .= ("last" :: String)])

    it "encodes backoff durations as exact integer nanoseconds" $ do
      let request = consumerConfigRequest
            [ withConsumerBackoff
                [ fromRational (1234567891 % 1000000000)
                , fromRational (1 % 1000000000)
                ]
            , withConsumerMaxRequestBatch maxBound
            ]
      validateConsumerConfigRequest request `shouldBe` Right ()
      eitherDecode (encode request) `shouldBe` Right (object
        [ "backoff" .= [1234567891 :: Integer, 1]
        , "max_batch" .= (maxBound :: Int)
        ])

    it "floors backoff at sub-nanosecond and Int64 boundaries" $ do
      let request = consumerConfigRequest
            [withConsumerBackoff [subNanosecond, oneNanosecond, maxInt64Nanoseconds]]
      validateConsumerConfigRequest request `shouldBe` Right ()
      eitherDecode (encode request) `shouldBe` Right (object
        ["backoff" .= [0 :: Integer, 1, toInteger (maxBound :: Int64)]])

    it "rejects negative and overflowing backoff durations" $ do
      validateConsumerConfigRequest
        (consumerConfigRequest [withConsumerBackoff [negativeSubNanosecond]])
        `shouldBe` Left (JetStreamDecodeError backoffRangeError)
      validateConsumerConfigRequest
        (consumerConfigRequest [withConsumerBackoff [overflowingNanoseconds]])
        `shouldBe` Left (JetStreamDecodeError backoffRangeError)

    it "rejects negative max request batch" $ do
      validateConsumerConfigRequest
        (consumerConfigRequest [withConsumerMaxRequestBatch (-1)])
        `shouldBe` Left (JetStreamDecodeError "consumer max request batch must be zero or greater")

    it "allows backoff no longer than positive max deliver" $ do
      validateConsumerConfigRequest (backoffWithMaxDeliver 2 [1]) `shouldBe` Right ()
      validateConsumerConfigRequest (backoffWithMaxDeliver 2 [1, 2]) `shouldBe` Right ()
      validateConsumerConfigRequest (backoffWithMaxDeliver 0 [1, 2, 3]) `shouldBe` Right ()
      validateConsumerConfigRequest (backoffWithMaxDeliver (-1) [1, 2, 3]) `shouldBe` Right ()
      validateConsumerConfigRequest (backoffWithMaxDeliver 2 [1, 2, 3])
        `shouldBe` Left (JetStreamDecodeError
          "consumer backoff length cannot exceed positive max deliver")

    it "omits empty backoff and zero max request batch values" $ do
      let request = consumerConfigRequest
            [ withConsumerBackoff []
            , withConsumerMaxRequestBatch 0
            ]
      validateConsumerConfigRequest request `shouldBe` Right ()
      eitherDecode (encode request) `shouldBe` Right (object [])

  describe "Consumer reset request JSON" $ do
    it "encodes an optional reset sequence" $ do
      eitherDecode (encode (consumerResetRequest [withConsumerResetSequence 7]))
        `shouldBe` Right (object ["seq" .= (7 :: Int)])

  describe "ConsumerInfo JSON" .
    it "decodes server responses into concrete fields" $ do
      eitherDecode consumerInfoJSON `shouldBe` Right consumerInfoFixture

  describe "ConsumerConfig response JSON" $ do
    it "normalizes absent backoff and zero max request batch to unset" $ do
      fmap configResourceLimits (eitherDecode zeroMaxBatchConsumerConfigJSON)
        `shouldBe` Right (Nothing, Nothing)

    it "round-trips backoff and max request batch" $ do
      eitherDecode (encode (consumerInfoConfig consumerInfoFixture))
        `shouldBe` Right (consumerInfoConfig consumerInfoFixture)

  describe "ConsumerResetResponse JSON" .
    it "decodes reset metadata with the updated consumer info" $ do
      fmap consumerResetResponseSequence (eitherDecode consumerResetJSON)
        `shouldBe` Right 7

  describe "Consumer list JSON" $ do
    it "decodes detailed consumer list responses" $ do
      eitherDecode consumerListJSON
        `shouldBe` Right ConsumerListResponse
          { consumerListTotal = 1
          , consumerListOffset = 0
          , consumerListLimit = 1024
          , consumerListConsumers = [consumerInfoFixture]
          }

    it "decodes consumer name responses" $ do
      eitherDecode consumerNamesJSON
        `shouldBe` Right ConsumerNamesResponse
          { consumerNamesTotal = 2
          , consumerNamesOffset = 0
          , consumerNamesLimit = 1024
          , consumerNamesConsumers = ["orders-puller", "orders-worker"]
          }

configResourceLimits config =
  (consumerConfigBackoff config, consumerConfigMaxRequestBatch config)

backoffWithMaxDeliver maxDeliver backoff =
  consumerConfigRequest
    [ withConsumerMaxDeliver maxDeliver
    , withConsumerBackoff backoff
    ]

subNanosecond = fromRational (1 % 2000000000)

negativeSubNanosecond = fromRational ((-1) % 2000000000)

oneNanosecond = fromRational (1 % 1000000000)

maxInt64Nanoseconds =
  fromRational (toInteger (maxBound :: Int64) % 1000000000)

overflowingNanoseconds =
  fromRational ((toInteger (maxBound :: Int64) + 1) % 1000000000)

backoffRangeError =
  "consumer backoff durations must floor to nanoseconds between 0 and 9223372036854775807"

durablePullConsumerConfigRequest :: ConsumerConfigRequest
durablePullConsumerConfigRequest =
  consumerConfigRequest
    [ withConsumerDurableName "orders-puller"
    , withConsumerName "orders-puller"
    , withConsumerDeliverPolicy DeliverAll
    , withConsumerAckPolicy AckExplicit
    , withConsumerReplayPolicy ReplayInstant
    , withConsumerFilter (ConsumerFilterSubjects ["orders.created", "orders.updated"])
    , withConsumerMaxDeliver 5
    , withConsumerAckWait 30
    , withConsumerMaxAckPending 256
    , withConsumerInactiveThreshold 60
    ]

durablePullConsumerConfigValue :: Value
durablePullConsumerConfigValue = object
  [ "durable_name" .= ("orders-puller" :: String)
  , "name" .= ("orders-puller" :: String)
  , "deliver_policy" .= ("all" :: String)
  , "ack_policy" .= ("explicit" :: String)
  , "replay_policy" .= ("instant" :: String)
  , "filter_subjects" .= ["orders.created" :: String, "orders.updated"]
  , "max_deliver" .= (5 :: Int)
  , "ack_wait" .= (30000000000 :: Integer)
  , "max_ack_pending" .= (256 :: Int)
  , "inactive_threshold" .= (60000000000 :: Integer)
  ]

pushConsumerConfigRequest :: ConsumerConfigRequest
pushConsumerConfigRequest =
  consumerConfigRequest
    [ withConsumerName "push"
    , withConsumerDeliverSubject "deliver"
    , withConsumerDeliverGroup "workers"
    , withConsumerAckPolicy AckExplicit
    , withConsumerMaxWaiting 512
    , withConsumerIdleHeartbeat 1
    , withConsumerHeadersOnly True
    , withConsumerReplicas 1
    , withConsumerMemoryStorage True
    ]

pushConsumerConfigValue :: Value
pushConsumerConfigValue = object
  [ "name" .= ("push" :: String)
  , "deliver_subject" .= ("deliver" :: String)
  , "deliver_group" .= ("workers" :: String)
  , "ack_policy" .= ("explicit" :: String)
  , "max_waiting" .= (512 :: Int)
  , "idle_heartbeat" .= (1000000000 :: Integer)
  , "headers_only" .= True
  , "num_replicas" .= (1 :: Int)
  , "mem_storage" .= True
  ]

targetKindConsumerConfigRequest :: ConsumerConfigRequest
targetKindConsumerConfigRequest =
  applyConsumerTarget (NamedConsumer "right") .
    applyConsumerKind (PushConsumer "deliver") $
    consumerConfigRequest
      [ withConsumerName "wrong"
      , withConsumerDeliverSubject "wrong"
      , withConsumerDeliverGroup "workers"
      ]

targetKindConsumerConfigValue :: Value
targetKindConsumerConfigValue = object
  [ "name" .= ("right" :: String)
  , "deliver_subject" .= ("deliver" :: String)
  , "deliver_group" .= ("workers" :: String)
  ]

consumerInfoJSON :: LBS.ByteString
consumerInfoJSON =
  "{\"type\":\"io.nats.jetstream.api.v1.consumer_info_response\",\"stream_name\":\"ORDERS\",\"name\":\"orders-puller\",\"created\":\"2024-01-01T00:00:00Z\",\"config\":{\"deliver_policy\":\"all\",\"ack_policy\":\"explicit\",\"replay_policy\":\"instant\",\"backoff\":[1000000001,2],\"max_batch\":128},\"delivered\":{\"consumer_seq\":2,\"stream_seq\":10},\"ack_floor\":{\"consumer_seq\":1,\"stream_seq\":9},\"num_ack_pending\":1,\"num_redelivered\":0,\"num_waiting\":0,\"num_pending\":3}"

zeroMaxBatchConsumerConfigJSON :: LBS.ByteString
zeroMaxBatchConsumerConfigJSON =
  "{\"deliver_policy\":\"all\",\"ack_policy\":\"explicit\",\"replay_policy\":\"instant\",\"max_batch\":0}"

consumerResetJSON :: LBS.ByteString
consumerResetJSON =
  "{\"type\":\"io.nats.jetstream.api.v1.consumer_reset_response\",\"stream_name\":\"ORDERS\",\"name\":\"orders-puller\",\"created\":\"2024-01-01T00:00:00Z\",\"config\":{\"deliver_policy\":\"all\",\"ack_policy\":\"explicit\",\"replay_policy\":\"instant\"},\"delivered\":{\"consumer_seq\":1,\"stream_seq\":7},\"ack_floor\":{\"consumer_seq\":0,\"stream_seq\":0},\"num_ack_pending\":0,\"num_redelivered\":0,\"num_waiting\":0,\"num_pending\":3,\"reset_seq\":7}"

consumerListJSON :: LBS.ByteString
consumerListJSON =
  "{\"type\":\"io.nats.jetstream.api.v1.consumer_list_response\",\"total\":1,\"offset\":0,\"limit\":1024,\"consumers\":[{\"type\":\"io.nats.jetstream.api.v1.consumer_info_response\",\"stream_name\":\"ORDERS\",\"name\":\"orders-puller\",\"created\":\"2024-01-01T00:00:00Z\",\"config\":{\"deliver_policy\":\"all\",\"ack_policy\":\"explicit\",\"replay_policy\":\"instant\",\"backoff\":[1000000001,2],\"max_batch\":128},\"delivered\":{\"consumer_seq\":2,\"stream_seq\":10},\"ack_floor\":{\"consumer_seq\":1,\"stream_seq\":9},\"num_ack_pending\":1,\"num_redelivered\":0,\"num_waiting\":0,\"num_pending\":3}]}"

consumerNamesJSON :: LBS.ByteString
consumerNamesJSON =
  "{\"type\":\"io.nats.jetstream.api.v1.consumer_names_response\",\"total\":2,\"offset\":0,\"limit\":1024,\"consumers\":[\"orders-puller\",\"orders-worker\"]}"

consumerInfoFixture :: ConsumerInfo
consumerInfoFixture = ConsumerInfo
  { consumerInfoStreamName = "ORDERS"
  , consumerInfoName = "orders-puller"
  , consumerInfoCreated = timestamp
  , consumerInfoConfig = ConsumerConfig
      { consumerConfigDurableName = Nothing
      , consumerConfigName = Nothing
      , consumerConfigDescription = Nothing
      , consumerConfigDeliverSubject = Nothing
      , consumerConfigDeliverGroup = Nothing
      , consumerConfigDeliverPolicy = DeliverAll
      , consumerConfigAckPolicy = AckExplicit
      , consumerConfigReplayPolicy = ReplayInstant
      , consumerConfigFilterSubject = Nothing
      , consumerConfigFilterSubjects = Nothing
      , consumerConfigAckWait = Nothing
      , consumerConfigMaxDeliver = Nothing
      , consumerConfigMaxWaiting = Nothing
      , consumerConfigMaxAckPending = Nothing
      , consumerConfigBackoff = Just [fromRational (1000000001 % 1000000000), fromRational (2 % 1000000000)]
      , consumerConfigMaxRequestBatch = Just 128
      , consumerConfigInactiveThreshold = Nothing
      , consumerConfigIdleHeartbeat = Nothing
      , consumerConfigHeadersOnly = Nothing
      , consumerConfigReplicas = Nothing
      , consumerConfigMemoryStorage = Nothing
      }
  , consumerInfoDelivered = ConsumerSequenceInfo
      { consumerSequenceConsumer = 2
      , consumerSequenceStream = 10
      , consumerSequenceLast = Nothing
      }
  , consumerInfoAckFloor = ConsumerSequenceInfo
      { consumerSequenceConsumer = 1
      , consumerSequenceStream = 9
      , consumerSequenceLast = Nothing
      }
  , consumerInfoNumAckPending = 1
  , consumerInfoNumRedelivered = 0
  , consumerInfoNumWaiting = 0
  , consumerInfoNumPending = 3
  }

timestamp = read "2024-01-01 00:00:00 UTC"
