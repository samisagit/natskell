{-# LANGUAGE OverloadedStrings #-}

module JetStream.Consumer.Types
  ( AckPolicy (..)
  , ConsumerConfig (..)
  , ConsumerInfo (..)
  , ConsumerListRequest (..)
  , ConsumerListResponse (..)
  , ConsumerNamesRequest (..)
  , ConsumerNamesResponse (..)
  , ConsumerSequenceInfo (..)
  , DeleteConsumerResponse (..)
  , DeliverPolicy (..)
  , ReplayPolicy (..)
  , emptyConsumerConfig
  , defaultConsumerListRequest
  , defaultConsumerNamesRequest
  ) where

import           Data.Aeson
import           Data.Aeson.Types (Pair, Parser)
import qualified Data.ByteString  as BS
import           Data.Maybe       (catMaybes)
import qualified Data.Text        as T
import           Data.Time.Clock  (NominalDiffTime, UTCTime)
import           JetStream.Types
    ( AckPolicy (..)
    , ConsumerName
    , DeliverPolicy (..)
    , ReplayPolicy (..)
    , StreamName
    , Subject
    , byteStringToJSON
    , diffTimeNanosToJSON
    , parseByteString
    )

data ConsumerConfig = ConsumerConfig
                        { consumerConfigDurableName :: Maybe ConsumerName
                        , consumerConfigName :: Maybe ConsumerName
                        , consumerConfigDescription :: Maybe BS.ByteString
                        , consumerConfigDeliverPolicy :: Maybe DeliverPolicy
                        , consumerConfigAckPolicy :: Maybe AckPolicy
                        , consumerConfigReplayPolicy :: Maybe ReplayPolicy
                        , consumerConfigFilterSubject :: Maybe Subject
                        , consumerConfigFilterSubjects :: Maybe [Subject]
                        , consumerConfigAckWait :: Maybe NominalDiffTime
                        , consumerConfigMaxDeliver :: Maybe Int
                        , consumerConfigMaxAckPending :: Maybe Int
                        , consumerConfigInactiveThreshold :: Maybe NominalDiffTime
                        }
  deriving (Eq, Show)

emptyConsumerConfig :: ConsumerConfig
emptyConsumerConfig =
  ConsumerConfig
    { consumerConfigDurableName = Nothing
    , consumerConfigName = Nothing
    , consumerConfigDescription = Nothing
    , consumerConfigDeliverPolicy = Nothing
    , consumerConfigAckPolicy = Nothing
    , consumerConfigReplayPolicy = Nothing
    , consumerConfigFilterSubject = Nothing
    , consumerConfigFilterSubjects = Nothing
    , consumerConfigAckWait = Nothing
    , consumerConfigMaxDeliver = Nothing
    , consumerConfigMaxAckPending = Nothing
    , consumerConfigInactiveThreshold = Nothing
    }

data ConsumerInfo = ConsumerInfo
                      { consumerInfoType           :: Maybe BS.ByteString
                      , consumerInfoStreamName     :: Maybe StreamName
                      , consumerInfoName           :: Maybe ConsumerName
                      , consumerInfoCreated        :: Maybe UTCTime
                      , consumerInfoConfig         :: Maybe ConsumerConfig
                      , consumerInfoDelivered      :: Maybe ConsumerSequenceInfo
                      , consumerInfoAckFloor       :: Maybe ConsumerSequenceInfo
                      , consumerInfoNumAckPending  :: Maybe Int
                      , consumerInfoNumRedelivered :: Maybe Int
                      , consumerInfoNumWaiting     :: Maybe Int
                      , consumerInfoNumPending     :: Maybe Integer
                      }
  deriving (Eq, Show)

data ConsumerSequenceInfo = ConsumerSequenceInfo
                              { consumerSequenceConsumer :: Maybe Integer
                              , consumerSequenceStream   :: Maybe Integer
                              , consumerSequenceLast     :: Maybe UTCTime
                              }
  deriving (Eq, Show)

newtype DeleteConsumerResponse = DeleteConsumerResponse { deleteConsumerSuccess :: Maybe Bool }
  deriving (Eq, Show)

newtype ConsumerListRequest = ConsumerListRequest { consumerListRequestOffset :: Maybe Int }
  deriving (Eq, Show)

defaultConsumerListRequest :: ConsumerListRequest
defaultConsumerListRequest =
  ConsumerListRequest
    { consumerListRequestOffset = Nothing
    }

newtype ConsumerNamesRequest = ConsumerNamesRequest { consumerNamesRequestOffset :: Maybe Int }
  deriving (Eq, Show)

defaultConsumerNamesRequest :: ConsumerNamesRequest
defaultConsumerNamesRequest =
  ConsumerNamesRequest
    { consumerNamesRequestOffset = Nothing
    }

data ConsumerListResponse = ConsumerListResponse
                              { consumerListTotal     :: Maybe Int
                              , consumerListOffset    :: Maybe Int
                              , consumerListLimit     :: Maybe Int
                              , consumerListConsumers :: Maybe [ConsumerInfo]
                              }
  deriving (Eq, Show)

data ConsumerNamesResponse = ConsumerNamesResponse
                               { consumerNamesTotal     :: Maybe Int
                               , consumerNamesOffset    :: Maybe Int
                               , consumerNamesLimit     :: Maybe Int
                               , consumerNamesConsumers :: Maybe [ConsumerName]
                               }
  deriving (Eq, Show)

instance ToJSON ConsumerConfig where
  toJSON config =
    object . catMaybes $
      [ maybeByteStringPair "durable_name" (consumerConfigDurableName config)
      , maybeByteStringPair "name" (consumerConfigName config)
      , maybeByteStringPair "description" (consumerConfigDescription config)
      , maybePair "ack_policy" (consumerConfigAckPolicy config)
      , maybePair "replay_policy" (consumerConfigReplayPolicy config)
      , maybeByteStringPair "filter_subject" (consumerConfigFilterSubject config)
      , maybeByteStringListPair "filter_subjects" (consumerConfigFilterSubjects config)
      , maybeJsonPair "ack_wait" (diffTimeNanosToJSON <$> consumerConfigAckWait config)
      , maybePair "max_deliver" (consumerConfigMaxDeliver config)
      , maybePair "max_ack_pending" (consumerConfigMaxAckPending config)
      , maybeJsonPair "inactive_threshold" (diffTimeNanosToJSON <$> consumerConfigInactiveThreshold config)
      ] ++ deliverPolicyPairs (consumerConfigDeliverPolicy config)

instance FromJSON ConsumerConfig where
  parseJSON = withObject "ConsumerConfig" $ \obj ->
    ConsumerConfig
      <$> parseOptionalByteStringField obj "durable_name"
      <*> parseOptionalByteStringField obj "name"
      <*> parseOptionalByteStringField obj "description"
      <*> parseOptionalDeliverPolicyField obj "deliver_policy"
      <*> obj .:? "ack_policy"
      <*> obj .:? "replay_policy"
      <*> parseOptionalByteStringField obj "filter_subject"
      <*> parseOptionalByteStringListField obj "filter_subjects"
      <*> parseOptionalDurationField obj "ack_wait"
      <*> obj .:? "max_deliver"
      <*> obj .:? "max_ack_pending"
      <*> parseOptionalDurationField obj "inactive_threshold"

instance FromJSON ConsumerInfo where
  parseJSON = withObject "ConsumerInfo" $ \obj ->
    ConsumerInfo
      <$> parseOptionalByteStringField obj "type"
      <*> parseOptionalByteStringField obj "stream_name"
      <*> parseOptionalByteStringField obj "name"
      <*> obj .:? "created"
      <*> obj .:? "config"
      <*> obj .:? "delivered"
      <*> obj .:? "ack_floor"
      <*> obj .:? "num_ack_pending"
      <*> obj .:? "num_redelivered"
      <*> obj .:? "num_waiting"
      <*> obj .:? "num_pending"

instance ToJSON ConsumerInfo where
  toJSON info =
    object . catMaybes $
      [ maybeByteStringPair "type" (consumerInfoType info)
      , maybeByteStringPair "stream_name" (consumerInfoStreamName info)
      , maybeByteStringPair "name" (consumerInfoName info)
      , maybePair "created" (consumerInfoCreated info)
      , maybePair "config" (consumerInfoConfig info)
      , maybePair "delivered" (consumerInfoDelivered info)
      , maybePair "ack_floor" (consumerInfoAckFloor info)
      , maybePair "num_ack_pending" (consumerInfoNumAckPending info)
      , maybePair "num_redelivered" (consumerInfoNumRedelivered info)
      , maybePair "num_waiting" (consumerInfoNumWaiting info)
      , maybePair "num_pending" (consumerInfoNumPending info)
      ]

instance FromJSON ConsumerSequenceInfo where
  parseJSON = withObject "ConsumerSequenceInfo" $ \obj ->
    ConsumerSequenceInfo
      <$> obj .:? "consumer_seq"
      <*> obj .:? "stream_seq"
      <*> obj .:? "last_active"

instance ToJSON ConsumerSequenceInfo where
  toJSON info =
    object . catMaybes $
      [ maybePair "consumer_seq" (consumerSequenceConsumer info)
      , maybePair "stream_seq" (consumerSequenceStream info)
      , maybePair "last_active" (consumerSequenceLast info)
      ]

instance FromJSON DeleteConsumerResponse where
  parseJSON = withObject "DeleteConsumerResponse" $ \obj ->
    DeleteConsumerResponse
      <$> obj .:? "success"

instance ToJSON ConsumerListRequest where
  toJSON request =
    object . catMaybes $
      [ maybePair "offset" (consumerListRequestOffset request)
      ]

instance FromJSON ConsumerListRequest where
  parseJSON = withObject "ConsumerListRequest" $ \obj ->
    ConsumerListRequest
      <$> obj .:? "offset"

instance ToJSON ConsumerNamesRequest where
  toJSON request =
    object . catMaybes $
      [ maybePair "offset" (consumerNamesRequestOffset request)
      ]

instance FromJSON ConsumerNamesRequest where
  parseJSON = withObject "ConsumerNamesRequest" $ \obj ->
    ConsumerNamesRequest
      <$> obj .:? "offset"

instance FromJSON ConsumerListResponse where
  parseJSON = withObject "ConsumerListResponse" $ \obj ->
    ConsumerListResponse
      <$> obj .:? "total"
      <*> obj .:? "offset"
      <*> obj .:? "limit"
      <*> obj .:? "consumers"

instance FromJSON ConsumerNamesResponse where
  parseJSON = withObject "ConsumerNamesResponse" $ \obj ->
    ConsumerNamesResponse
      <$> obj .:? "total"
      <*> obj .:? "offset"
      <*> obj .:? "limit"
      <*> parseOptionalByteStringListField obj "consumers"

maybeByteStringPair :: Key -> Maybe BS.ByteString -> Maybe Pair
maybeByteStringPair key =
  fmap (\value -> key .= byteStringToJSON value)

maybeByteStringListPair :: Key -> Maybe [BS.ByteString] -> Maybe Pair
maybeByteStringListPair key =
  fmap ((key .=) . map byteStringToJSON)

maybePair :: ToJSON value => Key -> Maybe value -> Maybe Pair
maybePair key =
  fmap (key .=)

maybeJsonPair :: Key -> Maybe Value -> Maybe Pair
maybeJsonPair key =
  fmap (key .=)

deliverPolicyPairs :: Maybe DeliverPolicy -> [Maybe Pair]
deliverPolicyPairs Nothing =
  []
deliverPolicyPairs (Just policy) =
  maybePair "deliver_policy" (Just policy) : extraPairs policy
  where
    extraPairs selectedPolicy =
      case selectedPolicy of
        DeliverByStartSequence sequenceNumber ->
          [maybePair "opt_start_seq" (Just sequenceNumber)]
        DeliverByStartTime startTime ->
          [maybePair "opt_start_time" (Just startTime)]
        _ ->
          []

parseOptionalDeliverPolicyField :: Object -> Key -> Parser (Maybe DeliverPolicy)
parseOptionalDeliverPolicyField obj key = do
  policyName <- obj .:? key :: Parser (Maybe T.Text)
  case policyName of
    Nothing ->
      pure Nothing
    Just "all" ->
      pure (Just DeliverAll)
    Just "last" ->
      pure (Just DeliverLast)
    Just "new" ->
      pure (Just DeliverNew)
    Just "by_start_sequence" ->
      fmap (Just . DeliverByStartSequence) (obj .: "opt_start_seq")
    Just "by_start_time" ->
      fmap (Just . DeliverByStartTime) (obj .: "opt_start_time")
    Just "last_per_subject" ->
      pure (Just DeliverLastPerSubject)
    Just value ->
      fail ("unknown deliver policy: " ++ T.unpack value)

parseOptionalByteStringField :: Object -> Key -> Parser (Maybe BS.ByteString)
parseOptionalByteStringField obj key = do
  value <- obj .:? key
  traverse parseByteString value

parseOptionalByteStringListField :: Object -> Key -> Parser (Maybe [BS.ByteString])
parseOptionalByteStringListField obj key = do
  value <- obj .:? key
  traverse (traverse parseByteString) value

parseOptionalDurationField :: Object -> Key -> Parser (Maybe NominalDiffTime)
parseOptionalDurationField obj key =
  fmap nanosToDiffTime <$> obj .:? key

nanosToDiffTime :: Integer -> NominalDiffTime
nanosToDiffTime nanoseconds =
  fromRational (toRational nanoseconds / 1000000000)
