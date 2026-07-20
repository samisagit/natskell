{-# LANGUAGE OverloadedStrings #-}

module JetStream.KeyValueSpec (spec) where

import           Data.Aeson               (eitherDecode, encode, object, (.=))
import           JetStream.KeyValue.Types
import qualified JetStream.Stream.Types   as Stream
import           JetStream.Types
    ( StorageType (FileStorage, MemoryStorage)
    )
import           Test.Hspec

spec :: Spec
spec = do
  describe "key-value identifiers" $ do
    it "accepts official bucket and key character sets" $ do
      validateKeyValueBucketName "CACHE_v1-2" `shouldBe` Right ()
      validateKeyValueKey "tenant/one.process_id=value" `shouldBe` Right ()
      validateKeyValuePattern "tenant.*.>" `shouldBe` Right ()
      validateKeyValuePattern ">" `shouldBe` Right ()

    it "rejects invalid buckets, keys, and search patterns" $ do
      validateKeyValueBucketName "" `shouldBe` Left (KeyValueInvalidBucketName "")
      validateKeyValueBucketName "cache.one" `shouldBe` Left (KeyValueInvalidBucketName "cache.one")
      validateKeyValueKey ".hidden" `shouldBe` Left (KeyValueInvalidKey ".hidden")
      validateKeyValueKey "trailing." `shouldBe` Left (KeyValueInvalidKey "trailing.")
      validateKeyValueKey "empty..token" `shouldBe` Left (KeyValueInvalidKey "empty..token")
      validateKeyValueKey "has space" `shouldBe` Left (KeyValueInvalidKey "has space")
      validateKeyValuePattern "empty..token" `shouldBe` Left (KeyValueInvalidPattern "empty..token")
      validateKeyValuePattern "tenant.>.*" `shouldBe` Left (KeyValueInvalidPattern "tenant.>.*")

  describe "key-value configuration" $ do
    it "uses JetStream key-value defaults and normalizes zero values" $ do
      let defaults = keyValueConfig "CACHE" []
          normalized = keyValueConfig "CACHE"
            [ withKeyValueMaxValueSize 0
            , withKeyValueHistory 0
            , withKeyValueMaxBytes 0
            , withKeyValueReplicas 0
            ]
      defaults `shouldBe` normalized
      keyValueConfigMaxValueSize defaults `shouldBe` (-1)
      keyValueConfigHistory defaults `shouldBe` 1
      keyValueConfigMaxBytes defaults `shouldBe` (-1)
      keyValueConfigStorage defaults `shouldBe` FileStorage
      keyValueConfigReplicas defaults `shouldBe` 1
      keyValueConfigTTL defaults `shouldBe` 0

    it "validates bounded history and resource numbers" $ do
      validateKeyValueConfig (keyValueConfig "CACHE" [withKeyValueHistory 64])
        `shouldBe` Right ()
      validateKeyValueConfig (keyValueConfig "CACHE" [withKeyValueHistory 65])
        `shouldBe` Left (KeyValueInvalidHistory 65)
      validateKeyValueConfig (keyValueConfig "CACHE" [withKeyValueMaxValueSize (-2)])
        `shouldBe` Left (KeyValueInvalidMaxValueSize (-2))
      validateKeyValueConfig (keyValueConfig "CACHE" [withKeyValueMaxBytes (-2)])
        `shouldBe` Left (KeyValueInvalidMaxBytes (-2))
      validateKeyValueConfig (keyValueConfig "CACHE" [withKeyValueTTL (-1)])
        `shouldBe` Left (KeyValueInvalidTTL (-1))
      validateKeyValueConfig (keyValueConfig "CACHE" [withKeyValueReplicas (-1)])
        `shouldBe` Left (KeyValueInvalidReplicas (-1))

    it "maps configuration to an official KV backing stream" $ do
      let config = keyValueConfig "CACHE"
            [ withKeyValueDescription "binary snapshots"
            , withKeyValueMaxValueSize 1048576
            , withKeyValueHistory 3
            , withKeyValueTTL 60
            , withKeyValueMaxBytes 10485760
            , withKeyValueStorage MemoryStorage
            , withKeyValueReplicas 2
            , withKeyValueCompression True
            ]
          request = Stream.streamConfigRequest
            (keyValueStreamName "CACHE")
            [keyValuePatternSubject "CACHE" ">"]
            (keyValueStreamOptions config)
      eitherDecode (encode request) `shouldBe` Right (object
        [ "name" .= ("KV_CACHE" :: String)
        , "subjects" .= ["$KV.CACHE.>" :: String]
        , "description" .= ("binary snapshots" :: String)
        , "retention" .= ("limits" :: String)
        , "storage" .= ("memory" :: String)
        , "discard" .= ("new" :: String)
        , "max_consumers" .= (-1 :: Int)
        , "max_msgs" .= (-1 :: Integer)
        , "max_msgs_per_subject" .= (3 :: Integer)
        , "max_bytes" .= (10485760 :: Integer)
        , "max_age" .= (60000000000 :: Integer)
        , "max_msg_size" .= (1048576 :: Int)
        , "num_replicas" .= (2 :: Int)
        , "duplicate_window" .= (60000000000 :: Integer)
        , "deny_delete" .= True
        , "allow_rollup_hdrs" .= True
        , "allow_direct" .= True
        , "compression" .= ("s2" :: String)
        ])

  describe "key-value entries" $ do
    it "decodes delete and purge tombstones from stored headers" $ do
      keyValueEntryOperation <$> keyValueEntryFromStreamMessage bucket "deleted" (message "deleted" "DEL")
        `shouldBe` Right KeyValueDelete
      keyValueEntryOperation <$> keyValueEntryFromStreamMessage bucket "purged" (message "purged" "PURGE")
        `shouldBe` Right KeyValuePurge
      keyValueEntryOperation <$> keyValueEntryFromStreamMessage bucket "expired" (marker "expired" "MaxAge")
        `shouldBe` Right KeyValuePurge
      keyValueEntryOperation <$> keyValueEntryFromStreamMessage bucket "removed" (marker "removed" "Remove")
        `shouldBe` Right KeyValueDelete

    it "rejects a revision whose stream subject belongs to another key" $ do
      keyValueEntryFromStreamMessage bucket "expected" (message "other" "PUT")
        `shouldBe` Left (KeyValueKeyNotFound "CACHE" "expected")

    it "rejects malformed stored headers" $ do
      case keyValueEntryFromStreamMessage bucket "broken" (brokenHeaders "broken") of
        Left (KeyValueDecodeError _) -> pure ()
        result -> expectationFailure ("expected header decode failure, got " ++ show result)

  describe "key-value watch options" $ do
    it "rejects history combined with updates-only" $ do
      validateKeyValueWatchConfig
        (keyValueWatchConfig [withKeyValueIncludeHistory, withKeyValueUpdatesOnly])
        `shouldBe` Left KeyValueInvalidWatchOptions
  where
    bucket = KeyValueBucket "CACHE"
    message key operation =
      Stream.StreamMessage
        { Stream.streamMessageSubject = keyValueSubject "CACHE" key
        , Stream.streamMessageSequence = 10
        , Stream.streamMessageHeadersRaw = Just
            ("NATS/1.0\r\nKV-Operation: " <> operation <> "\r\n\r\n")
        , Stream.streamMessagePayload = Just ""
        , Stream.streamMessageTime = read "2026-01-01 00:00:00 UTC"
        }
    marker key reason =
      Stream.StreamMessage
        { Stream.streamMessageSubject = keyValueSubject "CACHE" key
        , Stream.streamMessageSequence = 10
        , Stream.streamMessageHeadersRaw = Just
            ("NATS/1.0\r\nNats-Marker-Reason: " <> reason <> "\r\n\r\n")
        , Stream.streamMessagePayload = Just ""
        , Stream.streamMessageTime = read "2026-01-01 00:00:00 UTC"
        }
    brokenHeaders key =
      Stream.StreamMessage
        { Stream.streamMessageSubject = keyValueSubject "CACHE" key
        , Stream.streamMessageSequence = 10
        , Stream.streamMessageHeadersRaw = Just "not-a-nats-header"
        , Stream.streamMessagePayload = Just ""
        , Stream.streamMessageTime = read "2026-01-01 00:00:00 UTC"
        }
