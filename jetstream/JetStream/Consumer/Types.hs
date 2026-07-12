{-# LANGUAGE OverloadedStrings #-}

module JetStream.Consumer.Types
  ( AckPolicy (..)
  , ConsumerConfig (..)
  , ConsumerConfigOption
  , ConsumerConfigRequest
  , ConsumerFilter (..)
  , ConsumerInfo (..)
  , ConsumerListOption
  , ConsumerListResponse (..)
  , ConsumerNamesResponse (..)
  , ConsumerSequenceInfo (..)
  , DeleteConsumerResponse (..)
  , DeliverPolicy (..)
  , ReplayPolicy (..)
  , consumerConfigRequest
  , consumerListRequest
  , consumerNamesRequest
  , ensureDurableConsumerConfig
  , withConsumerAckPolicy
  , withConsumerAckWait
  , withConsumerDescription
  , withConsumerDurableName
  , withConsumerFilter
  , withConsumerInactiveThreshold
  , withConsumerListOffset
  , withConsumerMaxAckPending
  , withConsumerMaxDeliver
  , withConsumerName
  , withConsumerDeliverPolicy
  , withConsumerReplayPolicy
  ) where

import           Data.Aeson
import           Data.Aeson.Types (Pair, Parser)
import qualified Data.ByteString  as BS
import           Data.Maybe       (catMaybes)
import qualified Data.Text        as T
import           Data.Time.Clock  (NominalDiffTime, UTCTime)
import           JetStream.Types
    ( AckPolicy (..)
    , CallOption
    , ConsumerName
    , DeliverPolicy (..)
    , ReplayPolicy (..)
    , StreamName
    , Subject
    , applyCallOptions
    , byteStringToJSON
    , diffTimeNanosToJSON
    , parseByteString
    )

data ConsumerConfigRequest = ConsumerConfigRequest
                               { consumerConfigRequestDurableName :: Maybe ConsumerName
                               , consumerConfigRequestName :: Maybe ConsumerName
                               , consumerConfigRequestDescription :: Maybe BS.ByteString
                               , consumerConfigRequestDeliverPolicy :: Maybe DeliverPolicy
                               , consumerConfigRequestAckPolicy :: Maybe AckPolicy
                               , consumerConfigRequestReplayPolicy :: Maybe ReplayPolicy
                               , consumerConfigRequestFilter :: Maybe ConsumerFilter
                               , consumerConfigRequestAckWait :: Maybe NominalDiffTime
                               , consumerConfigRequestMaxDeliver :: Maybe Int
                               , consumerConfigRequestMaxAckPending :: Maybe Int
                               , consumerConfigRequestInactiveThreshold :: Maybe NominalDiffTime
                               }
  deriving (Eq, Show)

type ConsumerConfigOption = CallOption ConsumerConfigRequest

consumerConfigRequest :: [ConsumerConfigOption] -> ConsumerConfigRequest
consumerConfigRequest options =
  applyCallOptions options emptyConsumerConfigRequest

emptyConsumerConfigRequest :: ConsumerConfigRequest
emptyConsumerConfigRequest =
  ConsumerConfigRequest
    { consumerConfigRequestDurableName = Nothing
    , consumerConfigRequestName = Nothing
    , consumerConfigRequestDescription = Nothing
    , consumerConfigRequestDeliverPolicy = Nothing
    , consumerConfigRequestAckPolicy = Nothing
    , consumerConfigRequestReplayPolicy = Nothing
    , consumerConfigRequestFilter = Nothing
    , consumerConfigRequestAckWait = Nothing
    , consumerConfigRequestMaxDeliver = Nothing
    , consumerConfigRequestMaxAckPending = Nothing
    , consumerConfigRequestInactiveThreshold = Nothing
    }

ensureDurableConsumerConfig :: ConsumerName -> ConsumerConfigRequest -> ConsumerConfigRequest
ensureDurableConsumerConfig durable config =
  config
    { consumerConfigRequestDurableName = choose (consumerConfigRequestDurableName config)
    , consumerConfigRequestName = choose (consumerConfigRequestName config)
    }
  where
    choose Nothing = Just durable
    choose (Just existing)
      | BS.null existing = Just durable
      | otherwise = Just existing

withConsumerDurableName :: ConsumerName -> ConsumerConfigOption
withConsumerDurableName durable config =
  config { consumerConfigRequestDurableName = Just durable }

withConsumerName :: ConsumerName -> ConsumerConfigOption
withConsumerName name config =
  config { consumerConfigRequestName = Just name }

withConsumerDescription :: BS.ByteString -> ConsumerConfigOption
withConsumerDescription description config =
  config { consumerConfigRequestDescription = Just description }

withConsumerDeliverPolicy :: DeliverPolicy -> ConsumerConfigOption
withConsumerDeliverPolicy policy config =
  config { consumerConfigRequestDeliverPolicy = Just policy }

withConsumerAckPolicy :: AckPolicy -> ConsumerConfigOption
withConsumerAckPolicy policy config =
  config { consumerConfigRequestAckPolicy = Just policy }

withConsumerReplayPolicy :: ReplayPolicy -> ConsumerConfigOption
withConsumerReplayPolicy policy config =
  config { consumerConfigRequestReplayPolicy = Just policy }

data ConsumerFilter = ConsumerFilterSubject Subject
                    | ConsumerFilterSubjects [Subject]
  deriving (Eq, Show)

withConsumerFilter :: ConsumerFilter -> ConsumerConfigOption
withConsumerFilter consumerFilter config =
  config { consumerConfigRequestFilter = Just consumerFilter }

withConsumerAckWait :: NominalDiffTime -> ConsumerConfigOption
withConsumerAckWait ackWait config =
  config { consumerConfigRequestAckWait = Just ackWait }

withConsumerMaxDeliver :: Int -> ConsumerConfigOption
withConsumerMaxDeliver maxDeliver config =
  config { consumerConfigRequestMaxDeliver = Just maxDeliver }

withConsumerMaxAckPending :: Int -> ConsumerConfigOption
withConsumerMaxAckPending maxAckPending config =
  config { consumerConfigRequestMaxAckPending = Just maxAckPending }

withConsumerInactiveThreshold :: NominalDiffTime -> ConsumerConfigOption
withConsumerInactiveThreshold inactiveThreshold config =
  config { consumerConfigRequestInactiveThreshold = Just inactiveThreshold }

data ConsumerConfig = ConsumerConfig
                        { consumerConfigDurableName :: Maybe ConsumerName
                        , consumerConfigName :: Maybe ConsumerName
                        , consumerConfigDescription :: Maybe BS.ByteString
                        , consumerConfigDeliverPolicy :: DeliverPolicy
                        , consumerConfigAckPolicy :: AckPolicy
                        , consumerConfigReplayPolicy :: ReplayPolicy
                        , consumerConfigFilterSubject :: Maybe Subject
                        , consumerConfigFilterSubjects :: Maybe [Subject]
                        , consumerConfigAckWait :: Maybe NominalDiffTime
                        , consumerConfigMaxDeliver :: Maybe Int
                        , consumerConfigMaxAckPending :: Maybe Int
                        , consumerConfigInactiveThreshold :: Maybe NominalDiffTime
                        }
  deriving (Eq, Show)

data ConsumerInfo = ConsumerInfo
                      { consumerInfoStreamName     :: StreamName
                      , consumerInfoName           :: ConsumerName
                      , consumerInfoCreated        :: UTCTime
                      , consumerInfoConfig         :: ConsumerConfig
                      , consumerInfoDelivered      :: ConsumerSequenceInfo
                      , consumerInfoAckFloor       :: ConsumerSequenceInfo
                      , consumerInfoNumAckPending  :: Int
                      , consumerInfoNumRedelivered :: Int
                      , consumerInfoNumWaiting     :: Int
                      , consumerInfoNumPending     :: Integer
                      }
  deriving (Eq, Show)

data ConsumerSequenceInfo = ConsumerSequenceInfo
                              { consumerSequenceConsumer :: Integer
                              , consumerSequenceStream   :: Integer
                              , consumerSequenceLast     :: Maybe UTCTime
                              }
  deriving (Eq, Show)

newtype DeleteConsumerResponse = DeleteConsumerResponse { deleteConsumerSuccess :: Bool }
  deriving (Eq, Show)

newtype ConsumerListRequest = ConsumerListRequest { consumerListRequestOffset :: Maybe Int }
  deriving (Eq, Show)

type ConsumerListOption = CallOption ConsumerListRequest

consumerListRequest :: [ConsumerListOption] -> ConsumerListRequest
consumerListRequest options =
  applyCallOptions options defaultConsumerListRequest

consumerNamesRequest :: [ConsumerListOption] -> ConsumerListRequest
consumerNamesRequest =
  consumerListRequest

defaultConsumerListRequest :: ConsumerListRequest
defaultConsumerListRequest =
  ConsumerListRequest
    { consumerListRequestOffset = Nothing
    }

withConsumerListOffset :: Int -> ConsumerListOption
withConsumerListOffset offset request =
  request { consumerListRequestOffset = Just offset }

data ConsumerListResponse = ConsumerListResponse
                              { consumerListTotal     :: Int
                              , consumerListOffset    :: Int
                              , consumerListLimit     :: Int
                              , consumerListConsumers :: [ConsumerInfo]
                              }
  deriving (Eq, Show)

data ConsumerNamesResponse = ConsumerNamesResponse
                               { consumerNamesTotal     :: Int
                               , consumerNamesOffset    :: Int
                               , consumerNamesLimit     :: Int
                               , consumerNamesConsumers :: [ConsumerName]
                               }
  deriving (Eq, Show)

instance ToJSON ConsumerConfigRequest where
  toJSON config =
    object . catMaybes $
      [ maybeByteStringPair "durable_name" (consumerConfigRequestDurableName config)
      , maybeByteStringPair "name" (consumerConfigRequestName config)
      , maybeByteStringPair "description" (consumerConfigRequestDescription config)
      , maybePair "ack_policy" (consumerConfigRequestAckPolicy config)
      , maybePair "replay_policy" (consumerConfigRequestReplayPolicy config)
      , maybeJsonPair "ack_wait" (diffTimeNanosToJSON <$> consumerConfigRequestAckWait config)
      , maybePair "max_deliver" (consumerConfigRequestMaxDeliver config)
      , maybePair "max_ack_pending" (consumerConfigRequestMaxAckPending config)
      , maybeJsonPair "inactive_threshold" (diffTimeNanosToJSON <$> consumerConfigRequestInactiveThreshold config)
      ] ++ maybe [] deliverPolicyRequestPairs (consumerConfigRequestDeliverPolicy config)
        ++ maybe [] consumerFilterPairs (consumerConfigRequestFilter config)

instance ToJSON ConsumerConfig where
  toJSON config =
    object . catMaybes $
      [ maybeByteStringPair "durable_name" (consumerConfigDurableName config)
      , maybeByteStringPair "name" (consumerConfigName config)
      , maybeByteStringPair "description" (consumerConfigDescription config)
      , Just ("ack_policy" .= consumerConfigAckPolicy config)
      , Just ("replay_policy" .= consumerConfigReplayPolicy config)
      , maybeByteStringPair "filter_subject" (consumerConfigFilterSubject config)
      , maybeByteStringListPair "filter_subjects" (consumerConfigFilterSubjects config)
      , maybeJsonPair "ack_wait" (diffTimeNanosToJSON <$> consumerConfigAckWait config)
      , maybePair "max_deliver" (consumerConfigMaxDeliver config)
      , maybePair "max_ack_pending" (consumerConfigMaxAckPending config)
      , maybeJsonPair "inactive_threshold" (diffTimeNanosToJSON <$> consumerConfigInactiveThreshold config)
      ] ++ fmap Just (deliverPolicyPairs (consumerConfigDeliverPolicy config))

instance FromJSON ConsumerConfig where
  parseJSON = withObject "ConsumerConfig" $ \obj ->
    ConsumerConfig
      <$> parseOptionalByteStringField obj "durable_name"
      <*> parseOptionalByteStringField obj "name"
      <*> parseOptionalByteStringField obj "description"
      <*> parseDeliverPolicyField obj "deliver_policy"
      <*> obj .: "ack_policy"
      <*> obj .: "replay_policy"
      <*> parseOptionalByteStringField obj "filter_subject"
      <*> parseOptionalByteStringListField obj "filter_subjects"
      <*> parseOptionalDurationField obj "ack_wait"
      <*> obj .:? "max_deliver"
      <*> obj .:? "max_ack_pending"
      <*> parseOptionalDurationField obj "inactive_threshold"

instance FromJSON ConsumerInfo where
  parseJSON = withObject "ConsumerInfo" $ \obj ->
    ConsumerInfo
      <$> parseByteStringField obj "stream_name"
      <*> parseByteStringField obj "name"
      <*> obj .: "created"
      <*> obj .: "config"
      <*> obj .:? "delivered" .!= emptyConsumerSequenceInfo
      <*> obj .:? "ack_floor" .!= emptyConsumerSequenceInfo
      <*> obj .:? "num_ack_pending" .!= 0
      <*> obj .:? "num_redelivered" .!= 0
      <*> obj .:? "num_waiting" .!= 0
      <*> obj .:? "num_pending" .!= 0

instance ToJSON ConsumerInfo where
  toJSON info =
    object
      [ "stream_name" .= byteStringToJSON (consumerInfoStreamName info)
      , "name" .= byteStringToJSON (consumerInfoName info)
      , "created" .= consumerInfoCreated info
      , "config" .= consumerInfoConfig info
      , "delivered" .= consumerInfoDelivered info
      , "ack_floor" .= consumerInfoAckFloor info
      , "num_ack_pending" .= consumerInfoNumAckPending info
      , "num_redelivered" .= consumerInfoNumRedelivered info
      , "num_waiting" .= consumerInfoNumWaiting info
      , "num_pending" .= consumerInfoNumPending info
      ]

instance FromJSON ConsumerSequenceInfo where
  parseJSON = withObject "ConsumerSequenceInfo" $ \obj ->
    ConsumerSequenceInfo
      <$> obj .:? "consumer_seq" .!= 0
      <*> obj .:? "stream_seq" .!= 0
      <*> obj .:? "last_active"

instance ToJSON ConsumerSequenceInfo where
  toJSON info =
    object . catMaybes $
      [ Just ("consumer_seq" .= consumerSequenceConsumer info)
      , Just ("stream_seq" .= consumerSequenceStream info)
      , maybePair "last_active" (consumerSequenceLast info)
      ]

instance FromJSON DeleteConsumerResponse where
  parseJSON = withObject "DeleteConsumerResponse" $ \obj ->
    DeleteConsumerResponse
      <$> obj .:? "success" .!= False

instance ToJSON ConsumerListRequest where
  toJSON request =
    object . catMaybes $
      [ maybePair "offset" (consumerListRequestOffset request)
      ]

instance FromJSON ConsumerListResponse where
  parseJSON = withObject "ConsumerListResponse" $ \obj ->
    ConsumerListResponse
      <$> obj .: "total"
      <*> obj .: "offset"
      <*> obj .: "limit"
      <*> obj .:? "consumers" .!= []

instance FromJSON ConsumerNamesResponse where
  parseJSON = withObject "ConsumerNamesResponse" $ \obj ->
    ConsumerNamesResponse
      <$> obj .: "total"
      <*> obj .: "offset"
      <*> obj .: "limit"
      <*> parseOptionalByteStringListField obj "consumers" .!= []

byteStringPair :: Key -> BS.ByteString -> Pair
byteStringPair key value =
  key .= byteStringToJSON value

maybeByteStringPair :: Key -> Maybe BS.ByteString -> Maybe Pair
maybeByteStringPair key =
  fmap (byteStringPair key)

maybeByteStringListPair :: Key -> Maybe [BS.ByteString] -> Maybe Pair
maybeByteStringListPair key =
  fmap ((key .=) . map byteStringToJSON)

maybePair :: ToJSON value => Key -> Maybe value -> Maybe Pair
maybePair key =
  fmap (key .=)

maybeJsonPair :: Key -> Maybe Value -> Maybe Pair
maybeJsonPair key =
  fmap (key .=)

consumerFilterPairs :: ConsumerFilter -> [Maybe Pair]
consumerFilterPairs consumerFilter =
  case consumerFilter of
    ConsumerFilterSubject subject ->
      [maybeByteStringPair "filter_subject" (Just subject)]
    ConsumerFilterSubjects subjects ->
      [maybeByteStringListPair "filter_subjects" (Just subjects)]

deliverPolicyRequestPairs :: DeliverPolicy -> [Maybe Pair]
deliverPolicyRequestPairs policy =
  fmap Just (deliverPolicyPairs policy)

deliverPolicyPairs :: DeliverPolicy -> [Pair]
deliverPolicyPairs policy =
  "deliver_policy" .= policy : extraPairs policy
  where
    extraPairs selectedPolicy =
      case selectedPolicy of
        DeliverByStartSequence sequenceNumber ->
          ["opt_start_seq" .= sequenceNumber]
        DeliverByStartTime startTime ->
          ["opt_start_time" .= startTime]
        _ ->
          []

parseDeliverPolicyField :: Object -> Key -> Parser DeliverPolicy
parseDeliverPolicyField obj key = do
  policyName <- obj .: key :: Parser T.Text
  case policyName of
    "all" ->
      pure DeliverAll
    "last" ->
      pure DeliverLast
    "new" ->
      pure DeliverNew
    "by_start_sequence" ->
      fmap DeliverByStartSequence (obj .: "opt_start_seq")
    "by_start_time" ->
      fmap DeliverByStartTime (obj .: "opt_start_time")
    "last_per_subject" ->
      pure DeliverLastPerSubject
    value ->
      fail ("unknown deliver policy: " ++ T.unpack value)

parseByteStringField :: Object -> Key -> Parser BS.ByteString
parseByteStringField obj key =
  obj .: key >>= parseByteString

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

emptyConsumerSequenceInfo :: ConsumerSequenceInfo
emptyConsumerSequenceInfo =
  ConsumerSequenceInfo
    { consumerSequenceConsumer = 0
    , consumerSequenceStream = 0
    , consumerSequenceLast = Nothing
    }
