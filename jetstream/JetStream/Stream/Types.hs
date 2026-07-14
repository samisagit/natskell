{-# LANGUAGE OverloadedStrings #-}

module JetStream.Stream.Types
  ( StreamAPI (..)
  , Stream (..)
  , RetentionPolicy (..)
  , StorageType (..)
  , DiscardPolicy (..)
  , StreamConfig (..)
  , StreamConfigOption
  , streamConfigRequest
  , withRetention
  , withStorage
  , withDiscard
  , withMaxMessages
  , withMaxBytes
  , withMaxAge
  , withReplicas
  , withDuplicateWindow
  , withAllowDirect
  , PurgeStreamOption
  , purgeStreamRequest
  , withPurgeSubject
  , withPurgeSequence
  , withPurgeKeep
  , StreamListOption
  , streamListRequest
  , streamNamesRequest
  , withStreamListOffset
  , withStreamListSubject
  , StreamMessage (..)
  , StreamMessageSelector (..)
  , streamMessageGetRequest
  , StreamMessageDeleteMode (..)
  , streamMessageDeleteRequest
  , DeleteStreamMessageResponse (..)
  , StreamInfo (..)
  , StreamState (..)
  , StreamCluster (..)
  , StreamPeer (..)
  , StreamSourceInfo (..)
  , DeleteStreamResponse (..)
  , PurgeStreamResponse (..)
  , StreamListResponse (..)
  , StreamNamesResponse (..)
  , durationToNanoseconds
  , nanosecondsToDuration
  ) where

import           Data.Aeson
import           Data.Aeson.Types       (Pair, Parser)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as Base64
import           Data.Maybe             (catMaybes)
import           Data.Time.Clock        (NominalDiffTime, UTCTime)
import           JetStream.Error        (JetStreamError)
import           JetStream.Types
    ( CallOption
    , DiscardPolicy (..)
    , JetStreamRequestOption
    , Payload
    , RetentionPolicy (..)
    , Sequence
    , StorageType (..)
    , StreamName
    , Subject
    , applyCallOptions
    , byteStringToJSON
    , parseByteString
    )

-- | Stream management operations. The public module exposes this type
-- abstractly so operations can be added without changing its constructor.
data StreamAPI = StreamAPI
                   { create :: StreamName -> [Subject] -> [StreamConfigOption] -> [JetStreamRequestOption] -> IO (Either JetStreamError StreamInfo)
                   , createOrUpdate :: StreamName -> [Subject] -> [StreamConfigOption] -> [JetStreamRequestOption] -> IO (Either JetStreamError StreamInfo)
                   , update :: StreamName -> [Subject] -> [StreamConfigOption] -> [JetStreamRequestOption] -> IO (Either JetStreamError StreamInfo)
                   , info :: StreamName -> [JetStreamRequestOption] -> IO (Either JetStreamError StreamInfo)
                   , getMessage :: StreamName -> StreamMessageSelector -> [JetStreamRequestOption] -> IO (Either JetStreamError StreamMessage)
                   , deleteMessage :: StreamName -> Sequence -> StreamMessageDeleteMode -> [JetStreamRequestOption] -> IO (Either JetStreamError DeleteStreamMessageResponse)
                   , delete :: StreamName -> [JetStreamRequestOption] -> IO (Either JetStreamError DeleteStreamResponse)
                   , purge :: StreamName -> [PurgeStreamOption] -> [JetStreamRequestOption] -> IO (Either JetStreamError PurgeStreamResponse)
                   , list :: [StreamListOption] -> [JetStreamRequestOption] -> IO (Either JetStreamError StreamListResponse)
                   , names :: [StreamListOption] -> [JetStreamRequestOption] -> IO (Either JetStreamError StreamNamesResponse)
                   }

-- | A stable reference to a stream. It is intentionally just an identity;
-- operations remain on 'StreamAPI', so handles do not retain resources.
newtype Stream = Stream { streamName :: StreamName }
  deriving (Eq, Ord, Show)

data StreamConfigRequest = StreamConfigRequest
                             { streamConfigRequestName :: StreamName
                             , streamConfigRequestSubjects :: [Subject]
                             , streamConfigRequestRetention :: Maybe RetentionPolicy
                             , streamConfigRequestStorage :: Maybe StorageType
                             , streamConfigRequestDiscard :: Maybe DiscardPolicy
                             , streamConfigRequestMaxMessages :: Maybe Integer
                             , streamConfigRequestMaxBytes :: Maybe Integer
                             , streamConfigRequestMaxAge :: Maybe NominalDiffTime
                             , streamConfigRequestReplicas :: Maybe Int
                             , streamConfigRequestDuplicateWindow :: Maybe NominalDiffTime
                             , streamConfigRequestAllowDirect :: Maybe Bool
                             }
  deriving (Eq, Show)

type StreamConfigOption = CallOption StreamConfigRequest

streamConfigRequest :: StreamName -> [Subject] -> [StreamConfigOption] -> StreamConfigRequest
streamConfigRequest name subjects options =
  applyCallOptions options $
    StreamConfigRequest
      { streamConfigRequestName = name
      , streamConfigRequestSubjects = subjects
      , streamConfigRequestRetention = Nothing
      , streamConfigRequestStorage = Nothing
      , streamConfigRequestDiscard = Nothing
      , streamConfigRequestMaxMessages = Nothing
      , streamConfigRequestMaxBytes = Nothing
      , streamConfigRequestMaxAge = Nothing
      , streamConfigRequestReplicas = Nothing
      , streamConfigRequestDuplicateWindow = Nothing
      , streamConfigRequestAllowDirect = Nothing
      }

withRetention :: RetentionPolicy -> StreamConfigOption
withRetention retention config =
  config { streamConfigRequestRetention = Just retention }

withStorage :: StorageType -> StreamConfigOption
withStorage storage config =
  config { streamConfigRequestStorage = Just storage }

withDiscard :: DiscardPolicy -> StreamConfigOption
withDiscard discard config =
  config { streamConfigRequestDiscard = Just discard }

withMaxMessages :: Integer -> StreamConfigOption
withMaxMessages maxMessages config =
  config { streamConfigRequestMaxMessages = Just maxMessages }

withMaxBytes :: Integer -> StreamConfigOption
withMaxBytes maxBytes config =
  config { streamConfigRequestMaxBytes = Just maxBytes }

withMaxAge :: NominalDiffTime -> StreamConfigOption
withMaxAge maxAge config =
  config { streamConfigRequestMaxAge = Just maxAge }

withReplicas :: Int -> StreamConfigOption
withReplicas replicas config =
  config { streamConfigRequestReplicas = Just replicas }

withDuplicateWindow :: NominalDiffTime -> StreamConfigOption
withDuplicateWindow window config =
  config { streamConfigRequestDuplicateWindow = Just window }

withAllowDirect :: Bool -> StreamConfigOption
withAllowDirect allowDirect config =
  config { streamConfigRequestAllowDirect = Just allowDirect }

data StreamConfig = StreamConfig
                      { streamConfigName            :: StreamName
                      , streamConfigSubjects        :: Maybe [Subject]
                      , streamConfigRetention       :: RetentionPolicy
                      , streamConfigStorage         :: StorageType
                      , streamConfigDiscard         :: DiscardPolicy
                      , streamConfigMaxMessages     :: Integer
                      , streamConfigMaxBytes        :: Integer
                      , streamConfigMaxAge          :: NominalDiffTime
                      , streamConfigReplicas        :: Int
                      , streamConfigDuplicateWindow :: Maybe NominalDiffTime
                      , streamConfigAllowDirect     :: Bool
                      }
  deriving (Eq, Show)

data PurgeStreamRequest = PurgeStreamRequest
                            { purgeStreamSubject  :: Maybe Subject
                            , purgeStreamSequence :: Maybe Sequence
                            , purgeStreamKeep     :: Maybe Integer
                            }
  deriving (Eq, Show)

type PurgeStreamOption = CallOption PurgeStreamRequest

purgeStreamRequest :: [PurgeStreamOption] -> PurgeStreamRequest
purgeStreamRequest options =
  applyCallOptions options $
    PurgeStreamRequest
      { purgeStreamSubject = Nothing
      , purgeStreamSequence = Nothing
      , purgeStreamKeep = Nothing
      }

withPurgeSubject :: Subject -> PurgeStreamOption
withPurgeSubject subject request =
  request { purgeStreamSubject = Just subject }

withPurgeSequence :: Sequence -> PurgeStreamOption
withPurgeSequence sequenceNumber request =
  request { purgeStreamSequence = Just sequenceNumber }

withPurgeKeep :: Integer -> PurgeStreamOption
withPurgeKeep keep request =
  request { purgeStreamKeep = Just keep }

data StreamListRequest = StreamListRequest
                           { streamListRequestOffset  :: Maybe Int
                           , streamListRequestSubject :: Maybe Subject
                           }
  deriving (Eq, Show)

type StreamListOption = CallOption StreamListRequest

streamListRequest :: [StreamListOption] -> StreamListRequest
streamListRequest options =
  applyCallOptions options defaultStreamListRequest

streamNamesRequest :: [StreamListOption] -> StreamListRequest
streamNamesRequest =
  streamListRequest

defaultStreamListRequest :: StreamListRequest
defaultStreamListRequest =
  StreamListRequest
    { streamListRequestOffset = Nothing
    , streamListRequestSubject = Nothing
    }

withStreamListOffset :: Int -> StreamListOption
withStreamListOffset offset request =
  request { streamListRequestOffset = Just offset }

withStreamListSubject :: Subject -> StreamListOption
withStreamListSubject subject request =
  request { streamListRequestSubject = Just subject }

data StreamInfo = StreamInfo
                    { streamInfoConfig  :: StreamConfig
                    , streamInfoCreated :: UTCTime
                    , streamInfoState   :: StreamState
                    , streamInfoCluster :: Maybe StreamCluster
                    , streamInfoMirror  :: Maybe StreamSourceInfo
                    , streamInfoSources :: [StreamSourceInfo]
                    }
  deriving (Eq, Show)

data StreamState = StreamState
                     { streamStateMessages      :: Integer
                     , streamStateBytes         :: Integer
                     , streamStateFirstSequence :: Sequence
                     , streamStateFirstTime     :: UTCTime
                     , streamStateLastSequence  :: Sequence
                     , streamStateLastTime      :: UTCTime
                     , streamStateConsumerCount :: Int
                     , streamStateDeleted       :: [Sequence]
                     , streamStateNumDeleted    :: Integer
                     , streamStateNumSubjects   :: Integer
                     }
  deriving (Eq, Show)

data StreamCluster = StreamCluster
                       { streamClusterName     :: BS.ByteString
                       , streamClusterLeader   :: BS.ByteString
                       , streamClusterReplicas :: [StreamPeer]
                       }
  deriving (Eq, Show)

data StreamPeer = StreamPeer
                    { streamPeerName    :: BS.ByteString
                    , streamPeerCurrent :: Bool
                    , streamPeerOffline :: Bool
                    , streamPeerActive  :: NominalDiffTime
                    , streamPeerLag     :: Integer
                    }
  deriving (Eq, Show)

data StreamSourceInfo = StreamSourceInfo
                          { streamSourceInfoName          :: StreamName
                          , streamSourceInfoFilterSubject :: Subject
                          , streamSourceInfoLag           :: Integer
                          , streamSourceInfoActive        :: NominalDiffTime
                          }
  deriving (Eq, Show)

newtype DeleteStreamResponse = DeleteStreamResponse { deleteStreamSuccess :: Bool }
  deriving (Eq, Show)

data PurgeStreamResponse = PurgeStreamResponse
                             { purgeStreamSuccess :: Bool
                             , purgeStreamPurged  :: Integer
                             }
  deriving (Eq, Show)

data StreamListResponse = StreamListResponse
                            { streamListTotal   :: Int
                            , streamListOffset  :: Int
                            , streamListLimit   :: Int
                            , streamListStreams :: [StreamInfo]
                            }
  deriving (Eq, Show)

data StreamNamesResponse = StreamNamesResponse
                             { streamNamesTotal   :: Int
                             , streamNamesOffset  :: Int
                             , streamNamesLimit   :: Int
                             , streamNamesStreams :: [StreamName]
                             }
  deriving (Eq, Show)

data StreamMessage = StreamMessage
                       { streamMessageSubject    :: Subject
                       , streamMessageSequence   :: Sequence
                       , streamMessageHeadersRaw :: Maybe BS.ByteString
                       , streamMessagePayload    :: Maybe Payload
                       , streamMessageTime       :: UTCTime
                       }
  deriving (Eq, Show)

data StreamMessageSelector = StreamMessageBySequence Sequence
                           | LastStreamMessageForSubject Subject
                           | NextStreamMessageForSubject Subject
  deriving (Eq, Show)

newtype StreamMessageGetRequest = StreamMessageGetRequest { streamMessageGetSelector :: StreamMessageSelector }
  deriving (Eq, Show)

streamMessageGetRequest :: StreamMessageSelector -> StreamMessageGetRequest
streamMessageGetRequest =
  StreamMessageGetRequest

data StreamMessageDeleteMode = DeleteMessage | SecureDeleteMessage
  deriving (Eq, Show)

data StreamMessageDeleteRequest = StreamMessageDeleteRequest
                                    { streamMessageDeleteSequence :: Sequence
                                    , streamMessageDeleteNoErase  :: Maybe Bool
                                    }
  deriving (Eq, Show)

streamMessageDeleteRequest :: Sequence -> StreamMessageDeleteMode -> StreamMessageDeleteRequest
streamMessageDeleteRequest sequenceNumber mode =
  StreamMessageDeleteRequest
    { streamMessageDeleteSequence = sequenceNumber
    , streamMessageDeleteNoErase =
        case mode of
          DeleteMessage       -> Just True
          SecureDeleteMessage -> Nothing
    }

newtype DeleteStreamMessageResponse = DeleteStreamMessageResponse { deleteStreamMessageSuccess :: Bool }
  deriving (Eq, Show)

instance ToJSON StreamConfigRequest where
  toJSON config =
    object $
      [ byteStringPair "name" (streamConfigRequestName config)
      , byteStringListPair "subjects" (streamConfigRequestSubjects config)
      ] ++ catMaybes
        [ maybePair "retention" (streamConfigRequestRetention config)
        , maybePair "storage" (streamConfigRequestStorage config)
        , maybePair "discard" (streamConfigRequestDiscard config)
        , maybePair "max_msgs" (streamConfigRequestMaxMessages config)
        , maybePair "max_bytes" (streamConfigRequestMaxBytes config)
        , maybeDurationPair "max_age" (streamConfigRequestMaxAge config)
        , maybePair "num_replicas" (streamConfigRequestReplicas config)
        , maybeDurationPair "duplicate_window" (streamConfigRequestDuplicateWindow config)
        , maybePair "allow_direct" (streamConfigRequestAllowDirect config)
        ]

instance ToJSON StreamConfig where
  toJSON config =
    object . catMaybes $
      [ Just (byteStringPair "name" (streamConfigName config))
      , maybeByteStringListPair "subjects" (streamConfigSubjects config)
      , Just ("retention" .= streamConfigRetention config)
      , Just ("storage" .= streamConfigStorage config)
      , Just ("discard" .= streamConfigDiscard config)
      , Just ("max_msgs" .= streamConfigMaxMessages config)
      , Just ("max_bytes" .= streamConfigMaxBytes config)
      , Just ("max_age" .= durationToNanoseconds (streamConfigMaxAge config))
      , Just ("num_replicas" .= streamConfigReplicas config)
      , maybeDurationPair "duplicate_window" (streamConfigDuplicateWindow config)
      , Just ("allow_direct" .= streamConfigAllowDirect config)
      ]

instance FromJSON StreamConfig where
  parseJSON =
    withObject "StreamConfig" $ \value ->
      StreamConfig
        <$> parseByteStringField value "name"
        <*> parseOptionalByteStringListField value "subjects"
        <*> value .: "retention"
        <*> value .: "storage"
        <*> value .: "discard"
        <*> value .: "max_msgs"
        <*> value .: "max_bytes"
        <*> parseDurationField value "max_age"
        <*> value .: "num_replicas"
        <*> parseOptionalDurationField value "duplicate_window"
        <*> value .: "allow_direct"

instance ToJSON PurgeStreamRequest where
  toJSON request =
    object . catMaybes $
      [ maybeByteStringPair "filter" (purgeStreamSubject request)
      , maybePair "seq" (purgeStreamSequence request)
      , maybePair "keep" (purgeStreamKeep request)
      ]

instance ToJSON StreamListRequest where
  toJSON request =
    object . catMaybes $
      [ maybePair "offset" (streamListRequestOffset request)
      , maybeByteStringPair "subject" (streamListRequestSubject request)
      ]

instance ToJSON StreamMessageGetRequest where
  toJSON request =
    case streamMessageGetSelector request of
      StreamMessageBySequence sequenceNumber ->
        object ["seq" .= sequenceNumber]
      LastStreamMessageForSubject subject ->
        object [byteStringPair "last_by_subj" subject]
      NextStreamMessageForSubject subject ->
        object [byteStringPair "next_by_subj" subject]

instance ToJSON StreamMessageDeleteRequest where
  toJSON request =
    object . catMaybes $
      [ Just ("seq" .= streamMessageDeleteSequence request)
      , maybePair "no_erase" (streamMessageDeleteNoErase request)
      ]

instance ToJSON StreamInfo where
  toJSON info =
    object . catMaybes $
      [ Just ("config" .= streamInfoConfig info)
      , Just ("created" .= streamInfoCreated info)
      , Just ("state" .= streamInfoState info)
      , maybePair "cluster" (streamInfoCluster info)
      , maybePair "mirror" (streamInfoMirror info)
      , Just ("sources" .= streamInfoSources info)
      ]

instance FromJSON StreamInfo where
  parseJSON =
    withObject "StreamInfo" $ \value ->
      StreamInfo
        <$> value .: "config"
        <*> value .: "created"
        <*> value .: "state"
        <*> value .:? "cluster"
        <*> value .:? "mirror"
        <*> value .:? "sources" .!= []

instance ToJSON StreamState where
  toJSON state =
    object
      [ "messages" .= streamStateMessages state
      , "bytes" .= streamStateBytes state
      , "first_seq" .= streamStateFirstSequence state
      , "first_ts" .= streamStateFirstTime state
      , "last_seq" .= streamStateLastSequence state
      , "last_ts" .= streamStateLastTime state
      , "consumer_count" .= streamStateConsumerCount state
      , "deleted" .= streamStateDeleted state
      , "num_deleted" .= streamStateNumDeleted state
      , "num_subjects" .= streamStateNumSubjects state
      ]

instance FromJSON StreamState where
  parseJSON =
    withObject "StreamState" $ \value ->
      StreamState
        <$> value .: "messages"
        <*> value .: "bytes"
        <*> value .: "first_seq"
        <*> value .: "first_ts"
        <*> value .: "last_seq"
        <*> value .: "last_ts"
        <*> value .: "consumer_count"
        <*> value .:? "deleted" .!= []
        <*> value .:? "num_deleted" .!= 0
        <*> value .:? "num_subjects" .!= 0

instance ToJSON StreamCluster where
  toJSON cluster =
    object
      [ "name" .= byteStringToJSON (streamClusterName cluster)
      , "leader" .= byteStringToJSON (streamClusterLeader cluster)
      , "replicas" .= streamClusterReplicas cluster
      ]

instance FromJSON StreamCluster where
  parseJSON =
    withObject "StreamCluster" $ \value ->
      StreamCluster
        <$> parseOptionalByteStringField value "name" .!= ""
        <*> parseOptionalByteStringField value "leader" .!= ""
        <*> value .:? "replicas" .!= []

instance ToJSON StreamPeer where
  toJSON peer =
    object
      [ "name" .= byteStringToJSON (streamPeerName peer)
      , "current" .= streamPeerCurrent peer
      , "offline" .= streamPeerOffline peer
      , "active" .= durationToNanoseconds (streamPeerActive peer)
      , "lag" .= streamPeerLag peer
      ]

instance FromJSON StreamPeer where
  parseJSON =
    withObject "StreamPeer" $ \value ->
      StreamPeer
        <$> parseByteStringField value "name"
        <*> value .: "current"
        <*> value .:? "offline" .!= False
        <*> parseDurationField value "active"
        <*> value .:? "lag" .!= 0

instance ToJSON StreamSourceInfo where
  toJSON source =
    object
      [ "name" .= byteStringToJSON (streamSourceInfoName source)
      , "filter_subject" .= byteStringToJSON (streamSourceInfoFilterSubject source)
      , "lag" .= streamSourceInfoLag source
      , "active" .= durationToNanoseconds (streamSourceInfoActive source)
      ]

instance FromJSON StreamSourceInfo where
  parseJSON =
    withObject "StreamSourceInfo" $ \value ->
      StreamSourceInfo
        <$> parseByteStringField value "name"
        <*> parseOptionalByteStringField value "filter_subject" .!= ""
        <*> value .: "lag"
        <*> parseDurationField value "active"

instance ToJSON DeleteStreamResponse where
  toJSON response =
    object
      [ "success" .= deleteStreamSuccess response
      ]

instance FromJSON DeleteStreamResponse where
  parseJSON =
    withObject "DeleteStreamResponse" $ \value ->
      DeleteStreamResponse
        <$> value .:? "success" .!= False

instance ToJSON PurgeStreamResponse where
  toJSON response =
    object
      [ "success" .= purgeStreamSuccess response
      , "purged" .= purgeStreamPurged response
      ]

instance FromJSON PurgeStreamResponse where
  parseJSON =
    withObject "PurgeStreamResponse" $ \value ->
      PurgeStreamResponse
        <$> value .:? "success" .!= False
        <*> value .:? "purged" .!= 0

instance ToJSON StreamListResponse where
  toJSON response =
    object
      [ "total" .= streamListTotal response
      , "offset" .= streamListOffset response
      , "limit" .= streamListLimit response
      , "streams" .= streamListStreams response
      ]

instance FromJSON StreamListResponse where
  parseJSON =
    withObject "StreamListResponse" $ \value ->
      StreamListResponse
        <$> value .: "total"
        <*> value .: "offset"
        <*> value .: "limit"
        <*> value .:? "streams" .!= []

instance ToJSON StreamNamesResponse where
  toJSON response =
    object
      [ "total" .= streamNamesTotal response
      , "offset" .= streamNamesOffset response
      , "limit" .= streamNamesLimit response
      , "streams" .= map byteStringToJSON (streamNamesStreams response)
      ]

instance FromJSON StreamNamesResponse where
  parseJSON =
    withObject "StreamNamesResponse" $ \value ->
      StreamNamesResponse
        <$> value .: "total"
        <*> value .: "offset"
        <*> value .: "limit"
        <*> parseOptionalByteStringListField value "streams" .!= []

instance FromJSON StreamMessage where
  parseJSON =
    withObject "StreamMessageResponse" $ \value ->
      value .: "message" >>= parseStoredMessage

instance ToJSON StreamMessage where
  toJSON message =
    object . catMaybes $
      [ Just (byteStringPair "subject" (streamMessageSubject message))
      , Just ("seq" .= streamMessageSequence message)
      , maybeBase64Pair "hdrs" (streamMessageHeadersRaw message)
      , maybeBase64Pair "data" (streamMessagePayload message)
      , Just ("time" .= streamMessageTime message)
      ]

instance FromJSON DeleteStreamMessageResponse where
  parseJSON =
    withObject "DeleteStreamMessageResponse" $ \value ->
      DeleteStreamMessageResponse
        <$> value .:? "success" .!= False

instance ToJSON DeleteStreamMessageResponse where
  toJSON response =
    object
      [ "success" .= deleteStreamMessageSuccess response
      ]

parseStoredMessage :: Value -> Parser StreamMessage
parseStoredMessage =
  withObject "StreamMessage" $ \value ->
    StreamMessage
      <$> parseByteStringField value "subject"
      <*> value .: "seq"
      <*> parseOptionalBase64Field value "hdrs"
      <*> parseOptionalBase64Field value "data"
      <*> value .: "time"

durationToNanoseconds :: NominalDiffTime -> Integer
durationToNanoseconds duration =
  round (realToFrac duration * (1000000000 :: Double))

nanosecondsToDuration :: Integer -> NominalDiffTime
nanosecondsToDuration nanoseconds =
  fromRational (toRational nanoseconds / 1000000000)

byteStringPair :: Key -> BS.ByteString -> Pair
byteStringPair key value = key .= byteStringToJSON value

byteStringListPair :: Key -> [BS.ByteString] -> Pair
byteStringListPair key = (key .=) . map byteStringToJSON

maybeByteStringPair :: Key -> Maybe BS.ByteString -> Maybe Pair
maybeByteStringPair key = fmap (byteStringPair key)

maybeBase64Pair :: Key -> Maybe BS.ByteString -> Maybe Pair
maybeBase64Pair key =
  fmap ((key .=) . byteStringToJSON . Base64.encode)

maybeByteStringListPair :: Key -> Maybe [BS.ByteString] -> Maybe Pair
maybeByteStringListPair key = fmap (byteStringListPair key)

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

parseOptionalBase64Field :: Object -> Key -> Parser (Maybe BS.ByteString)
parseOptionalBase64Field value key = do
  encoded <- value .:? key
  traverse parseBase64 encoded
  where
    parseBase64 jsonValue = do
      bytes <- parseByteString jsonValue
      case Base64.decode bytes of
        Left err      -> fail err
        Right decoded -> pure decoded

parseOptionalByteStringListField :: Object -> Key -> Parser (Maybe [BS.ByteString])
parseOptionalByteStringListField value key = do
  byteStrings <- value .:? key
  traverse (traverse parseByteString) byteStrings

parseDurationField :: Object -> Key -> Parser NominalDiffTime
parseDurationField value key =
  nanosecondsToDuration <$> value .: key

parseOptionalDurationField :: Object -> Key -> Parser (Maybe NominalDiffTime)
parseOptionalDurationField value key =
  fmap nanosecondsToDuration <$> value .:? key
