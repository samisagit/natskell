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
  describe "ConsumerConfig JSON" $ do
    it "decodes durable pull consumer fields" $ do
      eitherDecode durablePullConsumerConfigJSON `shouldBe` Right durablePullConsumerConfig

    it "encodes only populated fields" $ do
      eitherDecode (encode durablePullConsumerConfig) `shouldBe` Right durablePullConsumerConfigValue

  describe "ConsumerInfo JSON" .
    it "decodes partial server responses" $ do
      eitherDecode partialConsumerInfoJSON `shouldBe` Right partialConsumerInfo

durablePullConsumerConfigJSON :: LBS.ByteString
durablePullConsumerConfigJSON =
  "{\"durable_name\":\"orders-puller\",\"name\":\"orders-puller\",\"deliver_policy\":\"all\",\"ack_policy\":\"explicit\",\"replay_policy\":\"instant\",\"filter_subject\":\"orders.created\",\"filter_subjects\":[\"orders.created\",\"orders.updated\"],\"max_deliver\":5,\"ack_wait\":30000000000,\"max_ack_pending\":256,\"inactive_threshold\":60000000000}"

durablePullConsumerConfig :: ConsumerConfig
durablePullConsumerConfig = emptyConsumerConfig
  { consumerConfigDurableName = Just "orders-puller"
  , consumerConfigName = Just "orders-puller"
  , consumerConfigDeliverPolicy = Just DeliverAll
  , consumerConfigAckPolicy = Just AckExplicit
  , consumerConfigReplayPolicy = Just ReplayInstant
  , consumerConfigFilterSubject = Just "orders.created"
  , consumerConfigFilterSubjects = Just ["orders.created", "orders.updated"]
  , consumerConfigMaxDeliver = Just 5
  , consumerConfigAckWait = Just 30
  , consumerConfigMaxAckPending = Just 256
  , consumerConfigInactiveThreshold = Just 60
  }

durablePullConsumerConfigValue :: Value
durablePullConsumerConfigValue = object
  [ "durable_name" .= ("orders-puller" :: String)
  , "name" .= ("orders-puller" :: String)
  , "deliver_policy" .= ("all" :: String)
  , "ack_policy" .= ("explicit" :: String)
  , "replay_policy" .= ("instant" :: String)
  , "filter_subject" .= ("orders.created" :: String)
  , "filter_subjects" .= ["orders.created" :: String, "orders.updated"]
  , "max_deliver" .= (5 :: Int)
  , "ack_wait" .= (30000000000 :: Integer)
  , "max_ack_pending" .= (256 :: Int)
  , "inactive_threshold" .= (60000000000 :: Integer)
  ]

partialConsumerInfoJSON :: LBS.ByteString
partialConsumerInfoJSON =
  "{\"type\":\"io.nats.jetstream.api.v1.consumer_info_response\",\"stream_name\":\"ORDERS\",\"name\":\"orders-puller\",\"config\":{\"ack_policy\":\"explicit\"},\"delivered\":{\"consumer_seq\":2,\"stream_seq\":10},\"num_ack_pending\":1}"

partialConsumerInfo :: ConsumerInfo
partialConsumerInfo = ConsumerInfo
  { consumerInfoType = Just "io.nats.jetstream.api.v1.consumer_info_response"
  , consumerInfoStreamName = Just "ORDERS"
  , consumerInfoName = Just "orders-puller"
  , consumerInfoCreated = Nothing
  , consumerInfoConfig = Just emptyConsumerConfig
      { consumerConfigAckPolicy = Just AckExplicit
      }
  , consumerInfoDelivered = Just ConsumerSequenceInfo
      { consumerSequenceConsumer = Just 2
      , consumerSequenceStream = Just 10
      , consumerSequenceLast = Nothing
      }
  , consumerInfoAckFloor = Nothing
  , consumerInfoNumAckPending = Just 1
  , consumerInfoNumRedelivered = Nothing
  , consumerInfoNumWaiting = Nothing
  , consumerInfoNumPending = Nothing
  }
