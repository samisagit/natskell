{-# LANGUAGE OverloadedStrings #-}

module JetStream.KeyValue.Types
  ( KeyValueAPI (..)
  , KeyValueBucket (..)
  , KeyValueBucketName
  , KeyValueKey
  , KeyValuePattern
  , KeyValueValue
  , KeyValueRevision
  , KeyValueOperation (..)
  , KeyValueEntry (..)
  , KeyValueConfig (..)
  , KeyValueConfigOption
  , KeyValueStatus (..)
  , KeyValueError (..)
  , KeyValueDeleteOption
  , KeyValueDeleteConfig (..)
  , KeyValueWatchOption
  , KeyValueWatchConfig (..)
  , KeyValueWatcher (..)
  , KeyValueWatchBatch (..)
  , KeyValuePurgeDeletesOption
  , KeyValuePurgeDeletesConfig (..)
  , keyValueConfig
  , validateKeyValueConfig
  , validateKeyValueBucketName
  , validateKeyValueKey
  , validateKeyValuePattern
  , keyValueStreamName
  , keyValueSubjectPrefix
  , keyValueSubject
  , keyValuePatternSubject
  , keyValueDuplicateWindow
  , keyValueStreamOptions
  , keyValueEntryFromStreamMessage
  , keyValueEntryFromMessage
  , keyValueDeleteConfig
  , keyValueWatchConfig
  , validateKeyValueWatchConfig
  , keyValuePurgeDeletesConfig
  , withKeyValueDescription
  , withKeyValueMaxValueSize
  , withKeyValueHistory
  , withKeyValueTTL
  , withKeyValueMaxBytes
  , withKeyValueStorage
  , withKeyValueReplicas
  , withKeyValueCompression
  , withKeyValueLastRevision
  , withKeyValueIncludeHistory
  , withKeyValueUpdatesOnly
  , withKeyValueIgnoreDeletes
  , withKeyValueMetadataOnly
  , withKeyValueDeleteMarkersOlderThan
  ) where

import           Control.Concurrent.STM  (TVar)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BC
import           Data.Char
    ( isAsciiLower
    , isAsciiUpper
    , isDigit
    , toLower
    )
import           Data.Int                (Int32)
import           Data.Maybe              (catMaybes, fromMaybe)
import           Data.Time.Clock         (NominalDiffTime, UTCTime)
import           Data.Word               (Word64)
import           JetStream.Error         (JetStreamError)
import           JetStream.Message.Types
    ( FetchOption
    , Message
    , OrderedConsumer
    , PullStatus
    , messageHeaders
    , messageMetadata
    , messageMetadataNumPending
    , messageMetadataStreamSequence
    , messageMetadataTimestamp
    , messagePayload
    , messageSubject
    )
import           JetStream.Stream.Types
    ( StorageType (FileStorage)
    , StreamCompression (S2Compression)
    , StreamConfigOption
    , StreamMessage
    , streamMessageHeadersRaw
    , streamMessagePayload
    , streamMessageSequence
    , streamMessageSubject
    , streamMessageTime
    , withAllowDirect
    , withAllowRollup
    , withCompression
    , withDenyDelete
    , withDescription
    , withDiscard
    , withDuplicateWindow
    , withMaxAge
    , withMaxBytes
    , withMaxConsumers
    , withMaxMessageSize
    , withMaxMessages
    , withMaxMessagesPerSubject
    , withReplicas
    , withRetention
    , withStorage
    )
import           JetStream.Types
    ( CallOption
    , DiscardPolicy (DiscardNew)
    , JetStreamRequestOption
    , RetentionPolicy (LimitsPolicy)
    , applyCallOptions
    )
import           Parser.Attoparsec       (parseHeaderBlock)

type KeyValueBucketName = BS.ByteString
type KeyValueKey = BS.ByteString
type KeyValuePattern = BS.ByteString
type KeyValueValue = BS.ByteString
type KeyValueRevision = Word64

data KeyValueAPI = KeyValueAPI
                     { createKeyValueBucket :: KeyValueBucketName -> [KeyValueConfigOption] -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueBucket)
                     , updateKeyValueBucket :: KeyValueBucketName -> [KeyValueConfigOption] -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueBucket)
                     , createOrUpdateKeyValueBucket :: KeyValueBucketName -> [KeyValueConfigOption] -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueBucket)
                     , lookupKeyValueBucket :: KeyValueBucketName -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueBucket)
                     , deleteKeyValueBucket :: KeyValueBucketName -> [JetStreamRequestOption] -> IO (Either KeyValueError ())
                     , listKeyValueBuckets :: [JetStreamRequestOption] -> IO (Either KeyValueError [KeyValueBucket])
                     , listKeyValueStatuses :: [JetStreamRequestOption] -> IO (Either KeyValueError [KeyValueStatus])
                     , getKeyValueStatus :: KeyValueBucket -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueStatus)
                     , getKeyValueEntry :: KeyValueBucket -> KeyValueKey -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueEntry)
                     , getKeyValueEntryRevision :: KeyValueBucket -> KeyValueKey -> KeyValueRevision -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueEntry)
                     , putKeyValueEntry :: KeyValueBucket -> KeyValueKey -> KeyValueValue -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueRevision)
                     , createKeyValueEntry :: KeyValueBucket -> KeyValueKey -> KeyValueValue -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueRevision)
                     , updateKeyValueEntry :: KeyValueBucket -> KeyValueKey -> KeyValueValue -> KeyValueRevision -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueRevision)
                     , deleteKeyValueEntry :: KeyValueBucket -> KeyValueKey -> [KeyValueDeleteOption] -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueRevision)
                     , purgeKeyValueEntry :: KeyValueBucket -> KeyValueKey -> [KeyValueDeleteOption] -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueRevision)
                     , watchKeyValues :: KeyValueBucket -> [KeyValuePattern] -> [KeyValueWatchOption] -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueWatcher)
                     , fetchKeyValueWatch :: KeyValueWatcher -> [FetchOption] -> [JetStreamRequestOption] -> IO (Either KeyValueError KeyValueWatchBatch)
                     , stopKeyValueWatch :: KeyValueWatcher -> [JetStreamRequestOption] -> IO (Either KeyValueError ())
                     , listKeyValueKeys :: KeyValueBucket -> [JetStreamRequestOption] -> IO (Either KeyValueError [KeyValueKey])
                     , getKeyValueHistory :: KeyValueBucket -> KeyValueKey -> [JetStreamRequestOption] -> IO (Either KeyValueError [KeyValueEntry])
                     , purgeDeletedKeyValueEntries :: KeyValueBucket -> [KeyValuePurgeDeletesOption] -> [JetStreamRequestOption] -> IO (Either KeyValueError ())
                     }

newtype KeyValueBucket = KeyValueBucket { keyValueBucketName :: KeyValueBucketName }
  deriving (Eq, Ord, Show)

data KeyValueOperation = KeyValuePut | KeyValueDelete | KeyValuePurge
  deriving (Eq, Show)

data KeyValueEntry = KeyValueEntry
                       { keyValueEntryBucket    :: KeyValueBucketName
                       , keyValueEntryKey       :: KeyValueKey
                       , keyValueEntryValue     :: KeyValueValue
                       , keyValueEntryRevision  :: KeyValueRevision
                       , keyValueEntryCreated   :: UTCTime
                       , keyValueEntryDelta     :: Integer
                       , keyValueEntryOperation :: KeyValueOperation
                       }
  deriving (Eq, Show)

data KeyValueConfig = KeyValueConfig
                        { keyValueConfigBucket       :: KeyValueBucketName
                        , keyValueConfigDescription  :: Maybe BS.ByteString
                        , keyValueConfigMaxValueSize :: Int32
                        , keyValueConfigHistory      :: Int
                        , keyValueConfigTTL          :: NominalDiffTime
                        , keyValueConfigMaxBytes     :: Integer
                        , keyValueConfigStorage      :: StorageType
                        , keyValueConfigReplicas     :: Int
                        , keyValueConfigCompression  :: Bool
                        }
  deriving (Eq, Show)

type KeyValueConfigOption = CallOption KeyValueConfig

data KeyValueStatus = KeyValueStatus
                        { keyValueStatusBucket :: KeyValueBucketName
                        , keyValueStatusValues :: Integer
                        , keyValueStatusBytes  :: Integer
                        , keyValueStatusConfig :: KeyValueConfig
                        }
  deriving (Eq, Show)

data KeyValueError = KeyValueJetStreamError JetStreamError
                   | KeyValueInvalidBucketName KeyValueBucketName
                   | KeyValueInvalidKey KeyValueKey
                   | KeyValueInvalidPattern KeyValuePattern
                   | KeyValueInvalidHistory Int
                   | KeyValueInvalidMaxValueSize Int32
                   | KeyValueInvalidMaxBytes Integer
                   | KeyValueInvalidTTL NominalDiffTime
                   | KeyValueInvalidReplicas Int
                   | KeyValueInvalidWatchOptions
                   | KeyValueBucketNotFound KeyValueBucketName
                   | KeyValueBucketExists KeyValueBucketName
                   | KeyValueInvalidBucket KeyValueBucketName
                   | KeyValueKeyNotFound KeyValueBucketName KeyValueKey
                   | KeyValueKeyExists KeyValueBucketName KeyValueKey
                   | KeyValueRevisionMismatch KeyValueBucketName KeyValueKey KeyValueRevision
                   | KeyValueNoKeysFound KeyValueBucketName
                   | KeyValueDecodeError String
  deriving (Eq, Show)

newtype KeyValueDeleteConfig = KeyValueDeleteConfig { keyValueDeleteExpectedRevision :: Maybe KeyValueRevision }
  deriving (Eq, Show)

type KeyValueDeleteOption = CallOption KeyValueDeleteConfig

data KeyValueWatchConfig = KeyValueWatchConfig
                             { keyValueWatchIncludeHistory :: Bool
                             , keyValueWatchUpdatesOnly    :: Bool
                             , keyValueWatchIgnoreDeletes  :: Bool
                             , keyValueWatchMetadataOnly   :: Bool
                             }
  deriving (Eq, Show)

type KeyValueWatchOption = CallOption KeyValueWatchConfig

data KeyValueWatcher = KeyValueWatcher
                         { keyValueWatcherBucket           :: KeyValueBucket
                         , keyValueWatcherConsumer         :: OrderedConsumer
                         , keyValueWatcherIgnoreDeletes    :: Bool
                         , keyValueWatcherInitialRemaining :: TVar Integer
                         , keyValueWatcherInitialComplete  :: TVar Bool
                         }

data KeyValueWatchBatch = KeyValueWatchBatch
                            { keyValueWatchEntries         :: [KeyValueEntry]
                            , keyValueWatchInitialComplete :: Bool
                            , keyValueWatchStatus          :: Maybe PullStatus
                            }
  deriving (Eq, Show)

newtype KeyValuePurgeDeletesConfig = KeyValuePurgeDeletesConfig { keyValueDeleteMarkersOlderThan :: NominalDiffTime }
  deriving (Eq, Show)

type KeyValuePurgeDeletesOption = CallOption KeyValuePurgeDeletesConfig

keyValueConfig :: KeyValueBucketName -> [KeyValueConfigOption] -> KeyValueConfig
keyValueConfig bucket options =
  normalizeKeyValueConfig $ applyCallOptions options KeyValueConfig
    { keyValueConfigBucket = bucket
    , keyValueConfigDescription = Nothing
    , keyValueConfigMaxValueSize = -1
    , keyValueConfigHistory = 1
    , keyValueConfigTTL = 0
    , keyValueConfigMaxBytes = -1
    , keyValueConfigStorage = FileStorage
    , keyValueConfigReplicas = 1
    , keyValueConfigCompression = False
    }

normalizeKeyValueConfig :: KeyValueConfig -> KeyValueConfig
normalizeKeyValueConfig config =
  config
    { keyValueConfigMaxValueSize = defaultWhenZero (-1) (keyValueConfigMaxValueSize config)
    , keyValueConfigHistory = defaultWhenZero 1 (keyValueConfigHistory config)
    , keyValueConfigMaxBytes = defaultWhenZero (-1) (keyValueConfigMaxBytes config)
    , keyValueConfigReplicas = defaultWhenZero 1 (keyValueConfigReplicas config)
    }
  where
    defaultWhenZero fallback value
      | value == 0 = fallback
      | otherwise = value

validateKeyValueConfig :: KeyValueConfig -> Either KeyValueError ()
validateKeyValueConfig config = do
  validateKeyValueBucketName (keyValueConfigBucket config)
  if keyValueConfigHistory config < 1 || keyValueConfigHistory config > 64
    then Left (KeyValueInvalidHistory (keyValueConfigHistory config))
    else Right ()
  if keyValueConfigMaxValueSize config < (-1)
    then Left (KeyValueInvalidMaxValueSize (keyValueConfigMaxValueSize config))
    else Right ()
  if keyValueConfigMaxBytes config < (-1)
    then Left (KeyValueInvalidMaxBytes (keyValueConfigMaxBytes config))
    else Right ()
  if keyValueConfigTTL config < 0
    then Left (KeyValueInvalidTTL (keyValueConfigTTL config))
    else Right ()
  if keyValueConfigReplicas config < 1
    then Left (KeyValueInvalidReplicas (keyValueConfigReplicas config))
    else Right ()

validateKeyValueBucketName :: KeyValueBucketName -> Either KeyValueError ()
validateKeyValueBucketName bucket
  | BS.null bucket = Left (KeyValueInvalidBucketName bucket)
  | BC.all isBucketCharacter bucket = Right ()
  | otherwise = Left (KeyValueInvalidBucketName bucket)

validateKeyValueKey :: KeyValueKey -> Either KeyValueError ()
validateKeyValueKey key
  | validKeyLike isKeyCharacter key = Right ()
  | otherwise = Left (KeyValueInvalidKey key)

validateKeyValuePattern :: KeyValuePattern -> Either KeyValueError ()
validateKeyValuePattern pattern'
  | validKeyLike isPatternCharacter pattern' && validGreaterThan pattern' = Right ()
  | otherwise = Left (KeyValueInvalidPattern pattern')
  where
    validGreaterThan value =
      case BC.elemIndex '>' value of
        Nothing    -> True
        Just index -> index == BS.length value - 1

validKeyLike :: (Char -> Bool) -> BS.ByteString -> Bool
validKeyLike validCharacter value =
  not (BS.null value)
    && BC.head value /= '.'
    && BC.last value /= '.'
    && not (".." `BS.isInfixOf` value)
    && BC.all validCharacter value

isBucketCharacter :: Char -> Bool
isBucketCharacter char =
  isAsciiAlphaNumeric char || char == '_' || char == '-'

isKeyCharacter :: Char -> Bool
isKeyCharacter char =
  isAsciiAlphaNumeric char || char `elem` ("-/_=." :: String)

isPatternCharacter :: Char -> Bool
isPatternCharacter char =
  isKeyCharacter char || char == '*' || char == '>'

isAsciiAlphaNumeric :: Char -> Bool
isAsciiAlphaNumeric char =
  isAsciiLower char || isAsciiUpper char || isDigit char

keyValueStreamName :: KeyValueBucketName -> BS.ByteString
keyValueStreamName = BS.append "KV_"

keyValueSubjectPrefix :: KeyValueBucketName -> BS.ByteString
keyValueSubjectPrefix bucket = BS.concat ["$KV.", bucket, "."]

keyValueSubject :: KeyValueBucketName -> KeyValueKey -> BS.ByteString
keyValueSubject bucket = BS.append (keyValueSubjectPrefix bucket)

keyValuePatternSubject :: KeyValueBucketName -> KeyValuePattern -> BS.ByteString
keyValuePatternSubject = keyValueSubject

keyValueStreamOptions :: KeyValueConfig -> [StreamConfigOption]
keyValueStreamOptions config =
  catMaybes
    [ withDescription <$> keyValueConfigDescription config
    , Just (withRetention LimitsPolicy)
    , Just (withStorage (keyValueConfigStorage config))
    , Just (withDiscard DiscardNew)
    , Just (withMaxConsumers (-1))
    , Just (withMaxMessages (-1))
    , Just (withMaxMessagesPerSubject (toInteger (keyValueConfigHistory config)))
    , Just (withMaxBytes (keyValueConfigMaxBytes config))
    , Just (withMaxAge (keyValueConfigTTL config))
    , Just (withMaxMessageSize (keyValueConfigMaxValueSize config))
    , Just (withReplicas (keyValueConfigReplicas config))
    , Just (withDuplicateWindow (keyValueDuplicateWindow config))
    , Just (withDenyDelete True)
    , Just (withAllowRollup True)
    , Just (withAllowDirect True)
    , if keyValueConfigCompression config
        then Just (withCompression S2Compression)
        else Nothing
    ]

keyValueDuplicateWindow :: KeyValueConfig -> NominalDiffTime
keyValueDuplicateWindow config
  | keyValueConfigTTL config > 0 = min 120 (keyValueConfigTTL config)
  | otherwise = 120

keyValueEntryFromStreamMessage
  :: KeyValueBucket
  -> KeyValueKey
  -> StreamMessage
  -> Either KeyValueError KeyValueEntry
keyValueEntryFromStreamMessage bucket key message
  | streamMessageSubject message /= expectedSubject =
      Left (KeyValueKeyNotFound bucketName key)
  | otherwise = do
      operation <- operationFromRawHeaders (streamMessageHeadersRaw message)
      Right KeyValueEntry
        { keyValueEntryBucket = bucketName
        , keyValueEntryKey = key
        , keyValueEntryValue = fromMaybe BS.empty (streamMessagePayload message)
        , keyValueEntryRevision = streamMessageSequence message
        , keyValueEntryCreated = streamMessageTime message
        , keyValueEntryDelta = 0
        , keyValueEntryOperation = operation
        }
  where
    bucketName = keyValueBucketName bucket
    expectedSubject = keyValueSubject bucketName key

keyValueEntryFromMessage
  :: KeyValueBucket
  -> Message
  -> Either KeyValueError KeyValueEntry
keyValueEntryFromMessage bucket message = do
  metadata <- maybe
    (Left (KeyValueDecodeError "missing JetStream message metadata"))
    Right
    (messageMetadata message)
  key <- keyFromSubject bucket (messageSubject message)
  pure KeyValueEntry
    { keyValueEntryBucket = keyValueBucketName bucket
    , keyValueEntryKey = key
    , keyValueEntryValue = messagePayload message
    , keyValueEntryRevision = messageMetadataStreamSequence metadata
    , keyValueEntryCreated = messageMetadataTimestamp metadata
    , keyValueEntryDelta = messageMetadataNumPending metadata
    , keyValueEntryOperation = operationFromHeaders (messageHeaders message)
    }

keyFromSubject :: KeyValueBucket -> BS.ByteString -> Either KeyValueError KeyValueKey
keyFromSubject bucket subject =
  if prefix `BS.isPrefixOf` subject && BS.length subject > BS.length prefix
    then Right (BS.drop (BS.length prefix) subject)
    else Left (KeyValueDecodeError "key-value message subject does not match bucket")
  where
    prefix = keyValueSubjectPrefix (keyValueBucketName bucket)

operationFromRawHeaders :: Maybe BS.ByteString -> Either KeyValueError KeyValueOperation
operationFromRawHeaders Nothing =
  Right KeyValuePut
operationFromRawHeaders (Just rawHeaders) =
  case parseHeaderBlock rawHeaders of
    Left err      -> Left (KeyValueDecodeError err)
    Right headers -> Right (operationFromHeaders (Just headers))

operationFromHeaders :: Maybe [(BS.ByteString, BS.ByteString)] -> KeyValueOperation
operationFromHeaders headers =
  case lookupHeader "KV-Operation" (fromMaybe [] headers) of
    Just "DEL"   -> KeyValueDelete
    Just "PURGE" -> KeyValuePurge
    _ ->
      case lookupHeader "Nats-Marker-Reason" (fromMaybe [] headers) of
        Just "MaxAge" -> KeyValuePurge
        Just "Purge"  -> KeyValuePurge
        Just "Remove" -> KeyValueDelete
        _             -> KeyValuePut

lookupHeader
  :: BS.ByteString
  -> [(BS.ByteString, BS.ByteString)]
  -> Maybe BS.ByteString
lookupHeader name = go
  where
    normalizedName = normalizeHeaderName name
    go [] = Nothing
    go ((headerName, value):headers)
      | normalizeHeaderName headerName == normalizedName = Just value
      | otherwise = go headers

normalizeHeaderName :: BS.ByteString -> BS.ByteString
normalizeHeaderName = BC.map toLower

keyValueDeleteConfig :: [KeyValueDeleteOption] -> KeyValueDeleteConfig
keyValueDeleteConfig options =
  applyCallOptions options (KeyValueDeleteConfig Nothing)

keyValueWatchConfig :: [KeyValueWatchOption] -> KeyValueWatchConfig
keyValueWatchConfig options =
  applyCallOptions options KeyValueWatchConfig
    { keyValueWatchIncludeHistory = False
    , keyValueWatchUpdatesOnly = False
    , keyValueWatchIgnoreDeletes = False
    , keyValueWatchMetadataOnly = False
    }

validateKeyValueWatchConfig :: KeyValueWatchConfig -> Either KeyValueError ()
validateKeyValueWatchConfig config
  | keyValueWatchIncludeHistory config && keyValueWatchUpdatesOnly config =
      Left KeyValueInvalidWatchOptions
  | otherwise = Right ()

keyValuePurgeDeletesConfig
  :: [KeyValuePurgeDeletesOption]
  -> KeyValuePurgeDeletesConfig
keyValuePurgeDeletesConfig options =
  applyCallOptions options (KeyValuePurgeDeletesConfig 1800)

withKeyValueDescription :: BS.ByteString -> KeyValueConfigOption
withKeyValueDescription description config =
  config { keyValueConfigDescription = Just description }

withKeyValueMaxValueSize :: Int32 -> KeyValueConfigOption
withKeyValueMaxValueSize maxValueSize config =
  config { keyValueConfigMaxValueSize = maxValueSize }

withKeyValueHistory :: Int -> KeyValueConfigOption
withKeyValueHistory history config =
  config { keyValueConfigHistory = history }

withKeyValueTTL :: NominalDiffTime -> KeyValueConfigOption
withKeyValueTTL ttl config =
  config { keyValueConfigTTL = ttl }

withKeyValueMaxBytes :: Integer -> KeyValueConfigOption
withKeyValueMaxBytes maxBytes config =
  config { keyValueConfigMaxBytes = maxBytes }

withKeyValueStorage :: StorageType -> KeyValueConfigOption
withKeyValueStorage storage config =
  config { keyValueConfigStorage = storage }

withKeyValueReplicas :: Int -> KeyValueConfigOption
withKeyValueReplicas replicas config =
  config { keyValueConfigReplicas = replicas }

withKeyValueCompression :: Bool -> KeyValueConfigOption
withKeyValueCompression compression config =
  config { keyValueConfigCompression = compression }

withKeyValueLastRevision :: KeyValueRevision -> KeyValueDeleteOption
withKeyValueLastRevision revision config =
  config { keyValueDeleteExpectedRevision = Just revision }

withKeyValueIncludeHistory :: KeyValueWatchOption
withKeyValueIncludeHistory config =
  config { keyValueWatchIncludeHistory = True }

withKeyValueUpdatesOnly :: KeyValueWatchOption
withKeyValueUpdatesOnly config =
  config { keyValueWatchUpdatesOnly = True }

withKeyValueIgnoreDeletes :: KeyValueWatchOption
withKeyValueIgnoreDeletes config =
  config { keyValueWatchIgnoreDeletes = True }

withKeyValueMetadataOnly :: KeyValueWatchOption
withKeyValueMetadataOnly config =
  config { keyValueWatchMetadataOnly = True }

withKeyValueDeleteMarkersOlderThan
  :: NominalDiffTime
  -> KeyValuePurgeDeletesOption
withKeyValueDeleteMarkersOlderThan olderThan config =
  config { keyValueDeleteMarkersOlderThan = olderThan }
