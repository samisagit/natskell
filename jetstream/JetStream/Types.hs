{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module JetStream.Types
  ( AccountAPIStats (..)
  , AccountInfo (..)
  , AccountLimits (..)
  , AccountTier (..)
  , StreamName
  , ConsumerName
  , Subject
  , Payload
  , Headers
  , CallOption
  , Sequence
  , sequenceFromWord64
  , sequenceToWord64
  , JetStreamRequestOption
  , applyRequestOptions
  , withRequestTimeout
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
import qualified Data.Aeson.Key     as Key
import qualified Data.Aeson.KeyMap  as KeyMap
import           Data.Aeson.Types   (Pair, Parser, typeMismatch)
import qualified Data.ByteString    as BS
import           Data.Maybe         (catMaybes, fromMaybe)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import           Data.Time.Clock    (NominalDiffTime)
import qualified Data.Time.Clock    as Time
import           Data.Word          (Word64)

type StreamName = BS.ByteString
type ConsumerName = BS.ByteString
type Subject = BS.ByteString
type Payload = BS.ByteString
type Headers = [(BS.ByteString, BS.ByteString)]
type CallOption a = a -> a

-- | A JetStream stream or consumer sequence number.
--
-- Sequence numbers are unsigned 64-bit values on the wire. The constructor is
-- intentionally private on the public API; use 'sequenceFromWord64' and
-- 'sequenceToWord64' at application boundaries.
newtype Sequence = Sequence Word64
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

sequenceFromWord64 :: Word64 -> Sequence
sequenceFromWord64 = Sequence

sequenceToWord64 :: Sequence -> Word64
sequenceToWord64 (Sequence value) = value

instance ToJSON Sequence where
  toJSON = toJSON . sequenceToWord64

instance FromJSON Sequence where
  parseJSON value = Sequence <$> parseJSON value

-- | Options scoped to one JetStream API request. This is distinct from
-- stream, consumer, publish, and fetch configuration.
newtype JetStreamRequestOption = JetStreamRequestOption (JetStreamRequestConfig -> JetStreamRequestConfig)

newtype JetStreamRequestConfig = JetStreamRequestConfig { requestConfigTimeoutMicros :: Maybe Int }

applyRequestOptions :: Int -> [JetStreamRequestOption] -> Int
applyRequestOptions defaultTimeout options =
  fromMaybe defaultTimeout
    (requestConfigTimeoutMicros (foldl apply emptyRequestConfig options))
  where
    emptyRequestConfig = JetStreamRequestConfig Nothing
    apply value (JetStreamRequestOption option) = option value

-- | Override the timeout for one JetStream request.
withRequestTimeout :: NominalDiffTime -> JetStreamRequestOption
withRequestTimeout timeoutSeconds =
  JetStreamRequestOption $ \config ->
    config
      { requestConfigTimeoutMicros =
          Just (max 1 (floor (realToFrac timeoutSeconds * (1000000 :: Double))))
      }

data AccountInfo = AccountInfo
                     { accountInfoTier   :: AccountTier
                     , accountInfoDomain :: Maybe BS.ByteString
                     , accountInfoAPI    :: AccountAPIStats
                     , accountInfoTiers  :: [(BS.ByteString, AccountTier)]
                     }
  deriving (Eq, Show)

data AccountTier = AccountTier
                     { accountTierMemory          :: Integer
                     , accountTierStorage         :: Integer
                     , accountTierReservedMemory  :: Integer
                     , accountTierReservedStorage :: Integer
                     , accountTierStreams         :: Int
                     , accountTierConsumers       :: Int
                     , accountTierLimits          :: AccountLimits
                     }
  deriving (Eq, Show)

data AccountAPIStats = AccountAPIStats
                         { accountAPILevel    :: Int
                         , accountAPITotal    :: Integer
                         , accountAPIErrors   :: Integer
                         , accountAPIInflight :: Maybe Integer
                         }
  deriving (Eq, Show)

data AccountLimits = AccountLimits
                       { accountLimitsMaxMemory             :: Integer
                       , accountLimitsMaxStorage            :: Integer
                       , accountLimitsMaxStreams            :: Int
                       , accountLimitsMaxConsumers          :: Int
                       , accountLimitsMaxAckPending         :: Int
                       , accountLimitsMemoryMaxStreamBytes  :: Integer
                       , accountLimitsStorageMaxStreamBytes :: Integer
                       , accountLimitsMaxBytesRequired      :: Bool
                       }
  deriving (Eq, Show)

applyCallOptions :: [CallOption a] -> a -> a
applyCallOptions options value =
  foldl (flip ($)) value options

data AckPolicy = AckNone
               | AckAll
               | AckExplicit
               | AckPolicyUnknown T.Text
  deriving (Eq, Show)

data DeliverPolicy = DeliverAll
                   | DeliverLast
                   | DeliverNew
                   | DeliverByStartSequence Sequence
                   | DeliverByStartTime Time.UTCTime
                   | DeliverLastPerSubject
                   | DeliverPolicyUnknown T.Text
  deriving (Eq, Show)

data DiscardPolicy = DiscardOld
                   | DiscardNew
                   | DiscardPolicyUnknown T.Text
  deriving (Eq, Show)

data ReplayPolicy = ReplayInstant
                  | ReplayOriginal
                  | ReplayPolicyUnknown T.Text
  deriving (Eq, Show)

data RetentionPolicy = LimitsPolicy
                     | InterestPolicy
                     | WorkQueuePolicy
                     | RetentionPolicyUnknown T.Text
  deriving (Eq, Show)

data StorageType = FileStorage
                 | MemoryStorage
                 | StorageTypeUnknown T.Text
  deriving (Eq, Show)

instance ToJSON AckPolicy where
  toJSON policy =
    String $
      case policy of
        AckNone                -> "none"
        AckAll                 -> "all"
        AckExplicit            -> "explicit"
        AckPolicyUnknown value -> value

instance FromJSON AckPolicy where
  parseJSON = withText "AckPolicy" $ \value ->
    case value of
      "none"     -> pure AckNone
      "all"      -> pure AckAll
      "explicit" -> pure AckExplicit
      _          -> pure (AckPolicyUnknown value)

instance ToJSON DeliverPolicy where
  toJSON policy =
    String $
      case policy of
        DeliverAll                 -> "all"
        DeliverLast                -> "last"
        DeliverNew                 -> "new"
        DeliverByStartSequence _   -> "by_start_sequence"
        DeliverByStartTime _       -> "by_start_time"
        DeliverLastPerSubject      -> "last_per_subject"
        DeliverPolicyUnknown value -> value

instance FromJSON DeliverPolicy where
  parseJSON = withText "DeliverPolicy" $ \value ->
    case value of
      "all"               -> pure DeliverAll
      "last"              -> pure DeliverLast
      "new"               -> pure DeliverNew
      "by_start_sequence" -> fail "deliver policy by_start_sequence requires opt_start_seq"
      "by_start_time"     -> fail "deliver policy by_start_time requires opt_start_time"
      "last_per_subject"  -> pure DeliverLastPerSubject
      _                   -> pure (DeliverPolicyUnknown value)

instance ToJSON DiscardPolicy where
  toJSON policy =
    String $
      case policy of
        DiscardOld                 -> "old"
        DiscardNew                 -> "new"
        DiscardPolicyUnknown value -> value

instance FromJSON DiscardPolicy where
  parseJSON = withText "DiscardPolicy" $ \value ->
    case value of
      "old" -> pure DiscardOld
      "new" -> pure DiscardNew
      _     -> pure (DiscardPolicyUnknown value)

instance ToJSON ReplayPolicy where
  toJSON policy =
    String $
      case policy of
        ReplayInstant             -> "instant"
        ReplayOriginal            -> "original"
        ReplayPolicyUnknown value -> value

instance FromJSON ReplayPolicy where
  parseJSON = withText "ReplayPolicy" $ \value ->
    case value of
      "instant"  -> pure ReplayInstant
      "original" -> pure ReplayOriginal
      _          -> pure (ReplayPolicyUnknown value)

instance ToJSON RetentionPolicy where
  toJSON policy =
    String $
      case policy of
        LimitsPolicy                 -> "limits"
        InterestPolicy               -> "interest"
        WorkQueuePolicy              -> "workqueue"
        RetentionPolicyUnknown value -> value

instance FromJSON RetentionPolicy where
  parseJSON = withText "RetentionPolicy" $ \value ->
    case value of
      "limits"    -> pure LimitsPolicy
      "interest"  -> pure InterestPolicy
      "workqueue" -> pure WorkQueuePolicy
      _           -> pure (RetentionPolicyUnknown value)

instance ToJSON StorageType where
  toJSON storage =
    String $
      case storage of
        FileStorage              -> "file"
        MemoryStorage            -> "memory"
        StorageTypeUnknown value -> value

instance FromJSON StorageType where
  parseJSON = withText "StorageType" $ \value ->
    case value of
      "file"   -> pure FileStorage
      "memory" -> pure MemoryStorage
      _        -> pure (StorageTypeUnknown value)

byteStringToJSON :: BS.ByteString -> Value
byteStringToJSON =
  String . E.decodeUtf8

parseByteString :: Value -> Parser BS.ByteString
parseByteString =
  withText "ByteString" (pure . E.encodeUtf8)

diffTimeNanosToJSON :: NominalDiffTime -> Value
diffTimeNanosToJSON diffTime =
  Number (fromInteger (floor (realToFrac diffTime * (1000000000 :: Double))))

instance FromJSON AccountInfo where
  parseJSON value@(Object obj) =
    AccountInfo
      <$> parseJSON value
      <*> parseOptionalByteStringField obj "domain"
      <*> obj .: "api"
      <*> parseAccountTiers obj
  parseJSON invalid =
    typeMismatch "AccountInfo" invalid

instance ToJSON AccountInfo where
  toJSON info =
    object $
      accountTierPairs (accountInfoTier info)
        ++ catMaybes
          [ maybeByteStringPair "domain" (accountInfoDomain info)
          , Just ("api" .= accountInfoAPI info)
          , Just ("tiers" .= accountTiersValue (accountInfoTiers info))
          ]

instance FromJSON AccountTier where
  parseJSON =
    withObject "AccountTier" $ \obj ->
      AccountTier
        <$> obj .: "memory"
        <*> obj .: "storage"
        <*> obj .: "reserved_memory"
        <*> obj .: "reserved_storage"
        <*> obj .: "streams"
        <*> obj .: "consumers"
        <*> obj .: "limits"

instance ToJSON AccountTier where
  toJSON =
    object . accountTierPairs

instance FromJSON AccountAPIStats where
  parseJSON =
    withObject "AccountAPIStats" $ \obj ->
      AccountAPIStats
        <$> obj .: "level"
        <*> obj .: "total"
        <*> obj .: "errors"
        <*> obj .:? "inflight"

instance ToJSON AccountAPIStats where
  toJSON stats =
    object . catMaybes $
      [ Just ("level" .= accountAPILevel stats)
      , Just ("total" .= accountAPITotal stats)
      , Just ("errors" .= accountAPIErrors stats)
      , maybePair "inflight" (accountAPIInflight stats)
      ]

instance FromJSON AccountLimits where
  parseJSON =
    withObject "AccountLimits" $ \obj ->
      AccountLimits
        <$> obj .: "max_memory"
        <*> obj .: "max_storage"
        <*> obj .: "max_streams"
        <*> obj .: "max_consumers"
        <*> obj .: "max_ack_pending"
        <*> obj .: "memory_max_stream_bytes"
        <*> obj .: "storage_max_stream_bytes"
        <*> obj .: "max_bytes_required"

instance ToJSON AccountLimits where
  toJSON limits =
    object
      [ "max_memory" .= accountLimitsMaxMemory limits
      , "max_storage" .= accountLimitsMaxStorage limits
      , "max_streams" .= accountLimitsMaxStreams limits
      , "max_consumers" .= accountLimitsMaxConsumers limits
      , "max_ack_pending" .= accountLimitsMaxAckPending limits
      , "memory_max_stream_bytes" .= accountLimitsMemoryMaxStreamBytes limits
      , "storage_max_stream_bytes" .= accountLimitsStorageMaxStreamBytes limits
      , "max_bytes_required" .= accountLimitsMaxBytesRequired limits
      ]

accountTierPairs :: AccountTier -> [Pair]
accountTierPairs tier =
  [ "memory" .= accountTierMemory tier
  , "storage" .= accountTierStorage tier
  , "reserved_memory" .= accountTierReservedMemory tier
  , "reserved_storage" .= accountTierReservedStorage tier
  , "streams" .= accountTierStreams tier
  , "consumers" .= accountTierConsumers tier
  , "limits" .= accountTierLimits tier
  ]

parseAccountTiers :: Object -> Parser [(BS.ByteString, AccountTier)]
parseAccountTiers obj = do
  tiers <- obj .:? "tiers"
  case tiers of
    Nothing ->
      pure []
    Just (Object tierMap) ->
      traverse parseTier (KeyMap.toList tierMap)
    Just invalid ->
      typeMismatch "AccountInfo.tiers" invalid
  where
    parseTier (name, value) =
      (,) (E.encodeUtf8 (Key.toText name)) <$> parseJSON value

accountTiersValue :: [(BS.ByteString, AccountTier)] -> Value
accountTiersValue =
  Object . KeyMap.fromList . fmap tierPair
  where
    tierPair (name, tier) =
      (Key.fromText (E.decodeUtf8 name), toJSON tier)

parseOptionalByteStringField :: Object -> Key.Key -> Parser (Maybe BS.ByteString)
parseOptionalByteStringField obj key = do
  value <- obj .:? key
  traverse parseByteString value

maybeByteStringPair :: Key.Key -> Maybe BS.ByteString -> Maybe Pair
maybeByteStringPair key =
  fmap ((key .=) . byteStringToJSON)

maybePair :: ToJSON value => Key.Key -> Maybe value -> Maybe Pair
maybePair key =
  fmap (key .=)
