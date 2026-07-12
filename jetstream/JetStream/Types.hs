{-# LANGUAGE OverloadedStrings #-}

module JetStream.Types
  ( StreamName
  , ConsumerName
  , Subject
  , Payload
  , Headers
  , CallOption
  , AckPolicy (..)
  , DeliverPolicy (..)
  , DiscardPolicy (..)
  , ReplayPolicy (..)
  , RetentionPolicy (..)
  , StorageType (..)
  , byteStringToJSON
  , parseByteString
  , diffTimeNanosToJSON
  , applyCallOptions
  ) where

import           Data.Aeson
import           Data.Aeson.Types   (Parser)
import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import           Data.Time.Clock    (NominalDiffTime)
import qualified Data.Time.Clock    as Time

type StreamName = BS.ByteString
type ConsumerName = BS.ByteString
type Subject = BS.ByteString
type Payload = BS.ByteString
type Headers = [(BS.ByteString, BS.ByteString)]
type CallOption a = a -> a

applyCallOptions :: [CallOption a] -> a -> a
applyCallOptions options value =
  foldr ($) value options

data AckPolicy = AckNone | AckAll | AckExplicit
  deriving (Eq, Show)

data DeliverPolicy = DeliverAll
                   | DeliverLast
                   | DeliverNew
                   | DeliverByStartSequence Int
                   | DeliverByStartTime Time.UTCTime
                   | DeliverLastPerSubject
  deriving (Eq, Show)

data DiscardPolicy = DiscardOld | DiscardNew
  deriving (Eq, Show)

data ReplayPolicy = ReplayInstant | ReplayOriginal
  deriving (Eq, Show)

data RetentionPolicy = LimitsPolicy | InterestPolicy | WorkQueuePolicy
  deriving (Eq, Show)

data StorageType = FileStorage | MemoryStorage
  deriving (Eq, Show)

instance ToJSON AckPolicy where
  toJSON policy =
    String $
      case policy of
        AckNone     -> "none"
        AckAll      -> "all"
        AckExplicit -> "explicit"

instance FromJSON AckPolicy where
  parseJSON = withText "AckPolicy" $ \value ->
    case value of
      "none"     -> pure AckNone
      "all"      -> pure AckAll
      "explicit" -> pure AckExplicit
      _          -> fail ("unknown ack policy: " ++ T.unpack value)

instance ToJSON DeliverPolicy where
  toJSON policy =
    String $
      case policy of
        DeliverAll               -> "all"
        DeliverLast              -> "last"
        DeliverNew               -> "new"
        DeliverByStartSequence _ -> "by_start_sequence"
        DeliverByStartTime _     -> "by_start_time"
        DeliverLastPerSubject    -> "last_per_subject"

instance FromJSON DeliverPolicy where
  parseJSON = withText "DeliverPolicy" $ \value ->
    case value of
      "all"               -> pure DeliverAll
      "last"              -> pure DeliverLast
      "new"               -> pure DeliverNew
      "by_start_sequence" -> pure (DeliverByStartSequence 0)
      "by_start_time"     -> pure DeliverAll
      "last_per_subject"  -> pure DeliverLastPerSubject
      _                   -> fail ("unknown deliver policy: " ++ T.unpack value)

instance ToJSON DiscardPolicy where
  toJSON policy =
    String $
      case policy of
        DiscardOld -> "old"
        DiscardNew -> "new"

instance FromJSON DiscardPolicy where
  parseJSON = withText "DiscardPolicy" $ \value ->
    case value of
      "old" -> pure DiscardOld
      "new" -> pure DiscardNew
      _     -> fail ("unknown discard policy: " ++ T.unpack value)

instance ToJSON ReplayPolicy where
  toJSON policy =
    String $
      case policy of
        ReplayInstant  -> "instant"
        ReplayOriginal -> "original"

instance FromJSON ReplayPolicy where
  parseJSON = withText "ReplayPolicy" $ \value ->
    case value of
      "instant"  -> pure ReplayInstant
      "original" -> pure ReplayOriginal
      _          -> fail ("unknown replay policy: " ++ T.unpack value)

instance ToJSON RetentionPolicy where
  toJSON policy =
    String $
      case policy of
        LimitsPolicy    -> "limits"
        InterestPolicy  -> "interest"
        WorkQueuePolicy -> "workqueue"

instance FromJSON RetentionPolicy where
  parseJSON = withText "RetentionPolicy" $ \value ->
    case value of
      "limits"    -> pure LimitsPolicy
      "interest"  -> pure InterestPolicy
      "workqueue" -> pure WorkQueuePolicy
      _           -> fail ("unknown retention policy: " ++ T.unpack value)

instance ToJSON StorageType where
  toJSON storage =
    String $
      case storage of
        FileStorage   -> "file"
        MemoryStorage -> "memory"

instance FromJSON StorageType where
  parseJSON = withText "StorageType" $ \value ->
    case value of
      "file"   -> pure FileStorage
      "memory" -> pure MemoryStorage
      _        -> fail ("unknown storage type: " ++ T.unpack value)

byteStringToJSON :: BS.ByteString -> Value
byteStringToJSON =
  String . E.decodeUtf8

parseByteString :: Value -> Parser BS.ByteString
parseByteString =
  withText "ByteString" (pure . E.encodeUtf8)

diffTimeNanosToJSON :: NominalDiffTime -> Value
diffTimeNanosToJSON diffTime =
  Number (fromInteger (floor (realToFrac diffTime * (1000000000 :: Double))))
