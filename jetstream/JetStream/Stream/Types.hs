{-# LANGUAGE OverloadedStrings #-}

module JetStream.Stream.Types
  ( RetentionPolicy (..)
  , StorageType (..)
  , DiscardPolicy (..)
  , StreamConfig (..)
  , PurgeStreamRequest (..)
  , defaultPurgeStreamRequest
  , StreamListRequest (..)
  , defaultStreamListRequest
  , StreamNamesRequest (..)
  , defaultStreamNamesRequest
  , StreamInfo (..)
  , StreamState (..)
  , StreamCluster (..)
  , StreamPeer (..)
  , StreamSourceInfo (..)
  , StreamAlternate (..)
  , DeleteStreamResponse (..)
  , PurgeStreamResponse (..)
  , StreamListResponse (..)
  , StreamNamesResponse (..)
  , durationToNanoseconds
  , nanosecondsToDuration
  ) where

import           Data.Aeson
import           Data.Aeson.Types (Pair, Parser)
import qualified Data.ByteString  as BS
import           Data.Maybe       (catMaybes)
import           Data.Time.Clock  (NominalDiffTime, UTCTime)
import           JetStream.Types
    ( DiscardPolicy (..)
    , RetentionPolicy (..)
    , StorageType (..)
    , StreamName
    , Subject
    , byteStringToJSON
    , parseByteString
    )

data StreamConfig = StreamConfig
                      { streamConfigName            :: StreamName
                      , streamConfigSubjects        :: [Subject]
                      , streamConfigRetention       :: Maybe RetentionPolicy
                      , streamConfigStorage         :: Maybe StorageType
                      , streamConfigDiscard         :: Maybe DiscardPolicy
                      , streamConfigMaxMessages     :: Maybe Integer
                      , streamConfigMaxBytes        :: Maybe Integer
                      , streamConfigMaxAge          :: Maybe NominalDiffTime
                      , streamConfigReplicas        :: Maybe Int
                      , streamConfigDuplicateWindow :: Maybe NominalDiffTime
                      , streamConfigAllowDirect     :: Maybe Bool
                      }
  deriving (Eq, Show)

data PurgeStreamRequest = PurgeStreamRequest
                            { purgeStreamSubject  :: Maybe Subject
                            , purgeStreamSequence :: Maybe Integer
                            , purgeStreamKeep     :: Maybe Integer
                            }
  deriving (Eq, Show)

defaultPurgeStreamRequest :: PurgeStreamRequest
defaultPurgeStreamRequest =
  PurgeStreamRequest
    { purgeStreamSubject = Nothing
    , purgeStreamSequence = Nothing
    , purgeStreamKeep = Nothing
    }

data StreamListRequest = StreamListRequest
                           { streamListRequestOffset  :: Maybe Int
                           , streamListRequestSubject :: Maybe Subject
                           }
  deriving (Eq, Show)

defaultStreamListRequest :: StreamListRequest
defaultStreamListRequest =
  StreamListRequest
    { streamListRequestOffset = Nothing
    , streamListRequestSubject = Nothing
    }

data StreamNamesRequest = StreamNamesRequest
                            { streamNamesRequestOffset  :: Maybe Int
                            , streamNamesRequestSubject :: Maybe Subject
                            }
  deriving (Eq, Show)

defaultStreamNamesRequest :: StreamNamesRequest
defaultStreamNamesRequest =
  StreamNamesRequest
    { streamNamesRequestOffset = Nothing
    , streamNamesRequestSubject = Nothing
    }

data StreamInfo = StreamInfo
                    { streamInfoType       :: Maybe BS.ByteString
                    , streamInfoConfig     :: Maybe StreamConfig
                    , streamInfoCreated    :: Maybe UTCTime
                    , streamInfoState      :: Maybe StreamState
                    , streamInfoCluster    :: Maybe StreamCluster
                    , streamInfoMirror     :: Maybe StreamSourceInfo
                    , streamInfoSources    :: Maybe [StreamSourceInfo]
                    , streamInfoAlternates :: Maybe [StreamAlternate]
                    }
  deriving (Eq, Show)

data StreamState = StreamState
                     { streamStateMessages      :: Maybe Integer
                     , streamStateBytes         :: Maybe Integer
                     , streamStateFirstSequence :: Maybe Integer
                     , streamStateFirstTime     :: Maybe UTCTime
                     , streamStateLastSequence  :: Maybe Integer
                     , streamStateLastTime      :: Maybe UTCTime
                     , streamStateConsumerCount :: Maybe Int
                     , streamStateDeleted       :: Maybe [Integer]
                     , streamStateNumDeleted    :: Maybe Integer
                     , streamStateNumSubjects   :: Maybe Integer
                     }
  deriving (Eq, Show)

data StreamCluster = StreamCluster
                       { streamClusterName     :: Maybe BS.ByteString
                       , streamClusterLeader   :: Maybe BS.ByteString
                       , streamClusterReplicas :: Maybe [StreamPeer]
                       }
  deriving (Eq, Show)

data StreamPeer = StreamPeer
                    { streamPeerName    :: Maybe BS.ByteString
                    , streamPeerCurrent :: Maybe Bool
                    , streamPeerOffline :: Maybe Bool
                    , streamPeerActive  :: Maybe NominalDiffTime
                    , streamPeerLag     :: Maybe Integer
                    }
  deriving (Eq, Show)

data StreamSourceInfo = StreamSourceInfo
                          { streamSourceInfoName :: Maybe StreamName
                          , streamSourceInfoFilterSubject :: Maybe Subject
                          , streamSourceInfoLag :: Maybe Integer
                          , streamSourceInfoActive :: Maybe NominalDiffTime
                          , streamSourceInfoError :: Maybe Value
                          }
  deriving (Eq, Show)

data StreamAlternate = StreamAlternate
                         { streamAlternateName    :: Maybe StreamName
                         , streamAlternateDomain  :: Maybe BS.ByteString
                         , streamAlternateCluster :: Maybe BS.ByteString
                         }
  deriving (Eq, Show)

newtype DeleteStreamResponse = DeleteStreamResponse { deleteStreamSuccess :: Maybe Bool }
  deriving (Eq, Show)

data PurgeStreamResponse = PurgeStreamResponse
                             { purgeStreamSuccess :: Maybe Bool
                             , purgeStreamPurged  :: Maybe Integer
                             }
  deriving (Eq, Show)

data StreamListResponse = StreamListResponse
                            { streamListTotal   :: Maybe Int
                            , streamListOffset  :: Maybe Int
                            , streamListLimit   :: Maybe Int
                            , streamListStreams :: Maybe [StreamInfo]
                            }
  deriving (Eq, Show)

data StreamNamesResponse = StreamNamesResponse
                             { streamNamesTotal   :: Maybe Int
                             , streamNamesOffset  :: Maybe Int
                             , streamNamesLimit   :: Maybe Int
                             , streamNamesStreams :: Maybe [StreamName]
                             }
  deriving (Eq, Show)

instance ToJSON StreamConfig where
  toJSON config =
    object $
      [ byteStringPair "name" (streamConfigName config)
      , "subjects" .= map byteStringToJSON (streamConfigSubjects config)
      ] ++ catMaybes
        [ maybePair "retention" (streamConfigRetention config)
        , maybePair "storage" (streamConfigStorage config)
        , maybePair "discard" (streamConfigDiscard config)
        , maybePair "max_msgs" (streamConfigMaxMessages config)
        , maybePair "max_bytes" (streamConfigMaxBytes config)
        , maybeDurationPair "max_age" (streamConfigMaxAge config)
        , maybePair "num_replicas" (streamConfigReplicas config)
        , maybeDurationPair "duplicate_window" (streamConfigDuplicateWindow config)
        , maybePair "allow_direct" (streamConfigAllowDirect config)
        ]

instance FromJSON StreamConfig where
  parseJSON =
    withObject "StreamConfig" $ \value ->
      StreamConfig
        <$> parseByteStringField value "name"
        <*> parseByteStringListField value "subjects"
        <*> value .:? "retention"
        <*> value .:? "storage"
        <*> value .:? "discard"
        <*> value .:? "max_msgs"
        <*> value .:? "max_bytes"
        <*> parseOptionalDurationField value "max_age"
        <*> value .:? "num_replicas"
        <*> parseOptionalDurationField value "duplicate_window"
        <*> value .:? "allow_direct"

instance ToJSON PurgeStreamRequest where
  toJSON request =
    object . catMaybes $
      [ maybeByteStringPair "filter" (purgeStreamSubject request)
      , maybePair "seq" (purgeStreamSequence request)
      , maybePair "keep" (purgeStreamKeep request)
      ]

instance FromJSON PurgeStreamRequest where
  parseJSON =
    withObject "PurgeStreamRequest" $ \value ->
      PurgeStreamRequest
        <$> parseOptionalByteStringField value "filter"
        <*> value .:? "seq"
        <*> value .:? "keep"

instance ToJSON StreamListRequest where
  toJSON request =
    object . catMaybes $
      [ maybePair "offset" (streamListRequestOffset request)
      , maybeByteStringPair "subject" (streamListRequestSubject request)
      ]

instance FromJSON StreamListRequest where
  parseJSON =
    withObject "StreamListRequest" $ \value ->
      StreamListRequest
        <$> value .:? "offset"
        <*> parseOptionalByteStringField value "subject"

instance ToJSON StreamNamesRequest where
  toJSON request =
    object . catMaybes $
      [ maybePair "offset" (streamNamesRequestOffset request)
      , maybeByteStringPair "subject" (streamNamesRequestSubject request)
      ]

instance FromJSON StreamNamesRequest where
  parseJSON =
    withObject "StreamNamesRequest" $ \value ->
      StreamNamesRequest
        <$> value .:? "offset"
        <*> parseOptionalByteStringField value "subject"

instance ToJSON StreamInfo where
  toJSON info =
    object . catMaybes $
      [ maybeByteStringPair "type" (streamInfoType info)
      , maybePair "config" (streamInfoConfig info)
      , maybePair "created" (streamInfoCreated info)
      , maybePair "state" (streamInfoState info)
      , maybePair "cluster" (streamInfoCluster info)
      , maybePair "mirror" (streamInfoMirror info)
      , maybePair "sources" (streamInfoSources info)
      , maybePair "alternates" (streamInfoAlternates info)
      ]

instance FromJSON StreamInfo where
  parseJSON =
    withObject "StreamInfo" $ \value ->
      StreamInfo
        <$> parseOptionalByteStringField value "type"
        <*> value .:? "config"
        <*> value .:? "created"
        <*> value .:? "state"
        <*> value .:? "cluster"
        <*> value .:? "mirror"
        <*> value .:? "sources"
        <*> value .:? "alternates"

instance ToJSON StreamState where
  toJSON state =
    object . catMaybes $
      [ maybePair "messages" (streamStateMessages state)
      , maybePair "bytes" (streamStateBytes state)
      , maybePair "first_seq" (streamStateFirstSequence state)
      , maybePair "first_ts" (streamStateFirstTime state)
      , maybePair "last_seq" (streamStateLastSequence state)
      , maybePair "last_ts" (streamStateLastTime state)
      , maybePair "consumer_count" (streamStateConsumerCount state)
      , maybePair "deleted" (streamStateDeleted state)
      , maybePair "num_deleted" (streamStateNumDeleted state)
      , maybePair "num_subjects" (streamStateNumSubjects state)
      ]

instance FromJSON StreamState where
  parseJSON =
    withObject "StreamState" $ \value ->
      StreamState
        <$> value .:? "messages"
        <*> value .:? "bytes"
        <*> value .:? "first_seq"
        <*> value .:? "first_ts"
        <*> value .:? "last_seq"
        <*> value .:? "last_ts"
        <*> value .:? "consumer_count"
        <*> value .:? "deleted"
        <*> value .:? "num_deleted"
        <*> value .:? "num_subjects"

instance ToJSON StreamCluster where
  toJSON cluster =
    object . catMaybes $
      [ maybeByteStringPair "name" (streamClusterName cluster)
      , maybeByteStringPair "leader" (streamClusterLeader cluster)
      , maybePair "replicas" (streamClusterReplicas cluster)
      ]

instance FromJSON StreamCluster where
  parseJSON =
    withObject "StreamCluster" $ \value ->
      StreamCluster
        <$> parseOptionalByteStringField value "name"
        <*> parseOptionalByteStringField value "leader"
        <*> value .:? "replicas"

instance ToJSON StreamPeer where
  toJSON peer =
    object . catMaybes $
      [ maybeByteStringPair "name" (streamPeerName peer)
      , maybePair "current" (streamPeerCurrent peer)
      , maybePair "offline" (streamPeerOffline peer)
      , maybeDurationPair "active" (streamPeerActive peer)
      , maybePair "lag" (streamPeerLag peer)
      ]

instance FromJSON StreamPeer where
  parseJSON =
    withObject "StreamPeer" $ \value ->
      StreamPeer
        <$> parseOptionalByteStringField value "name"
        <*> value .:? "current"
        <*> value .:? "offline"
        <*> parseOptionalDurationField value "active"
        <*> value .:? "lag"

instance ToJSON StreamSourceInfo where
  toJSON source =
    object . catMaybes $
      [ maybeByteStringPair "name" (streamSourceInfoName source)
      , maybeByteStringPair "filter_subject" (streamSourceInfoFilterSubject source)
      , maybePair "lag" (streamSourceInfoLag source)
      , maybeDurationPair "active" (streamSourceInfoActive source)
      , maybePair "error" (streamSourceInfoError source)
      ]

instance FromJSON StreamSourceInfo where
  parseJSON =
    withObject "StreamSourceInfo" $ \value ->
      StreamSourceInfo
        <$> parseOptionalByteStringField value "name"
        <*> parseOptionalByteStringField value "filter_subject"
        <*> value .:? "lag"
        <*> parseOptionalDurationField value "active"
        <*> value .:? "error"

instance ToJSON StreamAlternate where
  toJSON alternate =
    object . catMaybes $
      [ maybeByteStringPair "name" (streamAlternateName alternate)
      , maybeByteStringPair "domain" (streamAlternateDomain alternate)
      , maybeByteStringPair "cluster" (streamAlternateCluster alternate)
      ]

instance FromJSON StreamAlternate where
  parseJSON =
    withObject "StreamAlternate" $ \value ->
      StreamAlternate
        <$> parseOptionalByteStringField value "name"
        <*> parseOptionalByteStringField value "domain"
        <*> parseOptionalByteStringField value "cluster"

instance ToJSON DeleteStreamResponse where
  toJSON response =
    object . catMaybes $
      [ maybePair "success" (deleteStreamSuccess response)
      ]

instance FromJSON DeleteStreamResponse where
  parseJSON =
    withObject "DeleteStreamResponse" $ \value ->
      DeleteStreamResponse
        <$> value .:? "success"

instance ToJSON PurgeStreamResponse where
  toJSON response =
    object . catMaybes $
      [ maybePair "success" (purgeStreamSuccess response)
      , maybePair "purged" (purgeStreamPurged response)
      ]

instance FromJSON PurgeStreamResponse where
  parseJSON =
    withObject "PurgeStreamResponse" $ \value ->
      PurgeStreamResponse
        <$> value .:? "success"
        <*> value .:? "purged"

instance ToJSON StreamListResponse where
  toJSON response =
    object . catMaybes $
      [ maybePair "total" (streamListTotal response)
      , maybePair "offset" (streamListOffset response)
      , maybePair "limit" (streamListLimit response)
      , maybePair "streams" (streamListStreams response)
      ]

instance FromJSON StreamListResponse where
  parseJSON =
    withObject "StreamListResponse" $ \value ->
      StreamListResponse
        <$> value .:? "total"
        <*> value .:? "offset"
        <*> value .:? "limit"
        <*> value .:? "streams"

instance ToJSON StreamNamesResponse where
  toJSON response =
    object . catMaybes $
      [ maybePair "total" (streamNamesTotal response)
      , maybePair "offset" (streamNamesOffset response)
      , maybePair "limit" (streamNamesLimit response)
      , maybeByteStringListPair "streams" (streamNamesStreams response)
      ]

instance FromJSON StreamNamesResponse where
  parseJSON =
    withObject "StreamNamesResponse" $ \value ->
      StreamNamesResponse
        <$> value .:? "total"
        <*> value .:? "offset"
        <*> value .:? "limit"
        <*> parseOptionalByteStringListField value "streams"

durationToNanoseconds :: NominalDiffTime -> Integer
durationToNanoseconds duration =
  round (realToFrac duration * (1000000000 :: Double))

nanosecondsToDuration :: Integer -> NominalDiffTime
nanosecondsToDuration nanoseconds =
  fromRational (toRational nanoseconds / 1000000000)

byteStringPair :: Key -> BS.ByteString -> Pair
byteStringPair key value = key .= byteStringToJSON value

maybeByteStringPair :: Key -> Maybe BS.ByteString -> Maybe Pair
maybeByteStringPair key = fmap (byteStringPair key)

maybeByteStringListPair :: Key -> Maybe [BS.ByteString] -> Maybe Pair
maybeByteStringListPair key = fmap ((key .=) . map byteStringToJSON)

maybePair :: ToJSON value => Key -> Maybe value -> Maybe Pair
maybePair key = fmap (key .=)

maybeDurationPair :: Key -> Maybe NominalDiffTime -> Maybe Pair
maybeDurationPair key = fmap ((key .=) . durationToNanoseconds)

parseByteStringField :: Object -> Key -> Parser BS.ByteString
parseByteStringField value key =
  value .: key >>= parseByteString

parseOptionalByteStringField :: Object -> Key -> Parser (Maybe BS.ByteString)
parseOptionalByteStringField value key = do
  byteString <- value .:? key
  traverse parseByteString byteString

parseByteStringListField :: Object -> Key -> Parser [BS.ByteString]
parseByteStringListField value key = do
  byteStrings <- value .:? key .!= []
  traverse parseByteString byteStrings

parseOptionalByteStringListField :: Object -> Key -> Parser (Maybe [BS.ByteString])
parseOptionalByteStringListField value key = do
  byteStrings <- value .:? key
  traverse (traverse parseByteString) byteStrings

parseOptionalDurationField :: Object -> Key -> Parser (Maybe NominalDiffTime)
parseOptionalDurationField value key =
  fmap nanosecondsToDuration <$> value .:? key
