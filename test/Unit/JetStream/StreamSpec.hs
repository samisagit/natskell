{-# LANGUAGE OverloadedStrings #-}

module JetStream.StreamSpec (spec) where

import           Data.Aeson             (eitherDecode, encode, object, (.=))
import qualified Data.ByteString.Lazy   as LBS
import           Data.Int               (Int32)
import           JetStream.Error        (JetStreamError (JetStreamDecodeError))
import           JetStream.Stream.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe "StreamConfig request JSON" $ do
    it "omits max message size unless configured" $ do
      let request = streamConfigRequest "ORDERS" ["orders.>"] []
      eitherDecode (encode request) `shouldBe` Right (object
        [ "name" .= ("ORDERS" :: String)
        , "subjects" .= ["orders.>" :: String]
        ])

    it "encodes valid max message size boundaries" $ do
      let encodedMaxMessageSize value =
            eitherDecode . encode $
              streamConfigRequest "ORDERS" [] [withMaxMessageSize value]
          validatedMaxMessageSize value =
            validateStreamConfigRequest $
              streamConfigRequest "ORDERS" [] [withMaxMessageSize value]
          expected value = object
            [ "name" .= ("ORDERS" :: String)
            , "subjects" .= ([] :: [String])
            , "max_msg_size" .= value
            ]
      encodedMaxMessageSize (-1) `shouldBe` Right (expected (-1 :: Int32))
      encodedMaxMessageSize 0 `shouldBe` Right (expected (0 :: Int32))
      encodedMaxMessageSize maxBound `shouldBe` Right (expected (maxBound :: Int32))
      validatedMaxMessageSize (-1) `shouldBe` Right ()
      validatedMaxMessageSize 0 `shouldBe` Right ()
      validatedMaxMessageSize maxBound `shouldBe` Right ()

    it "rejects max message sizes below unlimited" $ do
      validateStreamConfigRequest
        (streamConfigRequest "ORDERS" [] [withMaxMessageSize (-2)])
        `shouldBe` Left (JetStreamDecodeError "stream max message size must be -1 or greater")
      validateStreamConfigRequest
        (streamConfigRequest "ORDERS" [] [withMaxMessageSize minBound])
        `shouldBe` Left (JetStreamDecodeError "stream max message size must be -1 or greater")

    it "encodes key-value backing stream controls" $ do
      let request = streamConfigRequest "KV_ORDERS" ["$KV.ORDERS.>"]
            [ withDescription "orders key-value bucket"
            , withMaxConsumers (-1)
            , withMaxMessagesPerSubject 3
            , withDenyDelete True
            , withAllowRollup True
            , withCompression S2Compression
            ]
      eitherDecode (encode request) `shouldBe` Right (object
        [ "name" .= ("KV_ORDERS" :: String)
        , "subjects" .= ["$KV.ORDERS.>" :: String]
        , "description" .= ("orders key-value bucket" :: String)
        , "max_consumers" .= (-1 :: Int)
        , "max_msgs_per_subject" .= (3 :: Integer)
        , "deny_delete" .= True
        , "allow_rollup_hdrs" .= True
        , "compression" .= ("s2" :: String)
        ])

  describe "StreamConfig response JSON" $ do
    it "normalizes a missing max message size to unlimited" $ do
      fmap streamConfigMaxMessageSize (eitherDecode streamConfigWithoutMaxMessageSizeJSON)
        `shouldBe` Right (-1)

    it "round-trips max message size" $ do
      eitherDecode (encode streamConfigFixture) `shouldBe` Right streamConfigFixture

streamConfigFixture :: StreamConfig
streamConfigFixture =
  StreamConfig
    { streamConfigName = "ORDERS"
    , streamConfigSubjects = Just ["orders.>"]
    , streamConfigDescription = Just "order events"
    , streamConfigRetention = LimitsPolicy
    , streamConfigStorage = MemoryStorage
    , streamConfigDiscard = DiscardOld
    , streamConfigMaxConsumers = -1
    , streamConfigMaxMessages = -1
    , streamConfigMaxMessagesPerSubject = 3
    , streamConfigMaxBytes = -1
    , streamConfigMaxAge = 0
    , streamConfigMaxMessageSize = maxBound
    , streamConfigReplicas = 1
    , streamConfigDuplicateWindow = Nothing
    , streamConfigDenyDelete = True
    , streamConfigAllowRollup = True
    , streamConfigAllowDirect = False
    , streamConfigCompression = S2Compression
    }

streamConfigWithoutMaxMessageSizeJSON :: LBS.ByteString
streamConfigWithoutMaxMessageSizeJSON =
  "{\"name\":\"ORDERS\",\"subjects\":[\"orders.>\"],\"retention\":\"limits\",\"storage\":\"memory\",\"discard\":\"old\",\"max_msgs\":-1,\"max_bytes\":-1,\"max_age\":0,\"num_replicas\":1,\"allow_direct\":false}"
