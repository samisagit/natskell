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

  describe "ConsumerInfo JSON" .
    it "decodes server responses into concrete fields" $ do
      eitherDecode consumerInfoJSON `shouldBe` Right consumerInfo

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

consumerInfoJSON :: LBS.ByteString
consumerInfoJSON =
  "{\"type\":\"io.nats.jetstream.api.v1.consumer_info_response\",\"stream_name\":\"ORDERS\",\"name\":\"orders-puller\",\"created\":\"2024-01-01T00:00:00Z\",\"config\":{\"deliver_policy\":\"all\",\"ack_policy\":\"explicit\",\"replay_policy\":\"instant\"},\"delivered\":{\"consumer_seq\":2,\"stream_seq\":10},\"ack_floor\":{\"consumer_seq\":1,\"stream_seq\":9},\"num_ack_pending\":1,\"num_redelivered\":0,\"num_waiting\":0,\"num_pending\":3}"

consumerInfo :: ConsumerInfo
consumerInfo = ConsumerInfo
  { consumerInfoStreamName = "ORDERS"
  , consumerInfoName = "orders-puller"
  , consumerInfoCreated = timestamp
  , consumerInfoConfig = ConsumerConfig
      { consumerConfigDurableName = Nothing
      , consumerConfigName = Nothing
      , consumerConfigDescription = Nothing
      , consumerConfigDeliverPolicy = DeliverAll
      , consumerConfigAckPolicy = AckExplicit
      , consumerConfigReplayPolicy = ReplayInstant
      , consumerConfigFilterSubject = Nothing
      , consumerConfigFilterSubjects = Nothing
      , consumerConfigAckWait = Nothing
      , consumerConfigMaxDeliver = Nothing
      , consumerConfigMaxAckPending = Nothing
      , consumerConfigInactiveThreshold = Nothing
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
