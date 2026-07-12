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
import           JetStream.Consumer.Types
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

  describe "Consumer reset request JSON" $ do
    it "encodes an optional reset sequence" $ do
      eitherDecode (encode (consumerResetRequest [withConsumerResetSequence 7]))
        `shouldBe` Right (object ["seq" .= (7 :: Int)])

  describe "ConsumerInfo JSON" .
    it "decodes server responses into concrete fields" $ do
      eitherDecode consumerInfoJSON `shouldBe` Right consumerInfo

  describe "ConsumerResetResponse JSON" .
    it "decodes reset metadata with the updated consumer info" $ do
      fmap consumerResetResponseSequence (eitherDecode consumerResetJSON)
        `shouldBe` Right 7

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
  "{\"type\":\"io.nats.jetstream.api.v1.consumer_info_response\",\"stream_name\":\"ORDERS\",\"name\":\"orders-puller\",\"created\":\"2024-01-01T00:00:00Z\",\"config\":{\"deliver_policy\":\"all\",\"ack_policy\":\"explicit\",\"replay_policy\":\"instant\"},\"delivered\":{\"consumer_seq\":2,\"stream_seq\":10},\"ack_floor\":{\"consumer_seq\":1,\"stream_seq\":9},\"num_ack_pending\":1,\"num_redelivered\":0,\"num_waiting\":0,\"num_pending\":3}"

consumerResetJSON :: LBS.ByteString
consumerResetJSON =
  "{\"type\":\"io.nats.jetstream.api.v1.consumer_reset_response\",\"stream_name\":\"ORDERS\",\"name\":\"orders-puller\",\"created\":\"2024-01-01T00:00:00Z\",\"config\":{\"deliver_policy\":\"all\",\"ack_policy\":\"explicit\",\"replay_policy\":\"instant\"},\"delivered\":{\"consumer_seq\":1,\"stream_seq\":7},\"ack_floor\":{\"consumer_seq\":0,\"stream_seq\":0},\"num_ack_pending\":0,\"num_redelivered\":0,\"num_waiting\":0,\"num_pending\":3,\"reset_seq\":7}"

consumerInfo :: ConsumerInfo
consumerInfo = ConsumerInfo
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
