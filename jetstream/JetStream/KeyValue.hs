{-# LANGUAGE OverloadedStrings #-}

module JetStream.KeyValue
  ( keyValueAPI
  , module JetStream.KeyValue.API
  ) where

import           Control.Concurrent.STM
    ( atomically
    , modifyTVar'
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTVar
    )
import           Control.Exception        (finally)
import           Control.Monad            (foldM, unless, void, when)
import qualified Data.ByteString          as BS
import           Data.List                (foldl', sort)
import           Data.Maybe               (catMaybes, fromMaybe, mapMaybe)
import           Data.Time.Clock          (diffUTCTime, getCurrentTime)
import           JetStream.Consumer.Types
    ( ConsumerFilter (ConsumerFilterSubject, ConsumerFilterSubjects)
    , consumerInfoNumPending
    )
import           JetStream.Error
    ( JetStreamApiError (apiErrorCodeDetail)
    , JetStreamError (JetStreamApiFailure)
    )
import           JetStream.KeyValue.API
import           JetStream.KeyValue.Types
import qualified JetStream.Message.API    as Message
import           JetStream.Message.Types
    ( FetchWait (FetchNoWaitMicros)
    , pullResponseMessages
    , pullResponseStatus
    , withFetchBatch
    , withFetchWait
    , withOrderedConsumerDeliverPolicy
    , withOrderedConsumerFilter
    , withOrderedConsumerHeadersOnly
    )
import qualified JetStream.Publish.API    as Publish
import qualified JetStream.Stream.API     as Stream
import           JetStream.Stream.Types
    ( StreamCompression (S2Compression)
    , StreamConfig
    , StreamInfo
    , StreamMessageSelector (LastStreamMessageForSubject, StreamMessageBySequence)
    , streamConfigAllowDirect
    , streamConfigAllowRollup
    , streamConfigCompression
    , streamConfigDenyDelete
    , streamConfigDescription
    , streamConfigDiscard
    , streamConfigDuplicateWindow
    , streamConfigMaxAge
    , streamConfigMaxBytes
    , streamConfigMaxConsumers
    , streamConfigMaxMessageSize
    , streamConfigMaxMessages
    , streamConfigMaxMessagesPerSubject
    , streamConfigName
    , streamConfigReplicas
    , streamConfigRetention
    , streamConfigStorage
    , streamConfigSubjects
    , streamInfoConfig
    , streamInfoState
    , streamListOffset
    , streamListStreams
    , streamListTotal
    , streamNamesOffset
    , streamNamesStreams
    , streamNamesTotal
    , streamStateBytes
    , streamStateMessages
    )
import           JetStream.Types
    ( DeliverPolicy (DeliverAll, DeliverLastPerSubject, DeliverNew)
    , DiscardPolicy (DiscardNew)
    , JetStreamRequestOption
    , RetentionPolicy (LimitsPolicy)
    )

keyValueAPI :: Stream.StreamAPI -> Publish.PublishAPI -> Message.MessageAPI -> KeyValueAPI
keyValueAPI streamAPI publishAPI messageAPI =
  KeyValueAPI
    { createKeyValueBucket = createBucket streamAPI
    , updateKeyValueBucket = configureBucket (Stream.update streamAPI)
    , createOrUpdateKeyValueBucket = configureBucket (Stream.createOrUpdate streamAPI)
    , lookupKeyValueBucket = lookupBucket streamAPI
    , deleteKeyValueBucket = deleteBucket streamAPI
    , listKeyValueBuckets = listBuckets streamAPI
    , listKeyValueStatuses = listStatuses streamAPI
    , getKeyValueStatus = bucketStatus streamAPI
    , getKeyValueEntry = getLatestEntry streamAPI
    , getKeyValueEntryRevision = getEntryRevision streamAPI
    , putKeyValueEntry = putEntry publishAPI
    , createKeyValueEntry = createEntry streamAPI publishAPI
    , updateKeyValueEntry = updateEntry publishAPI
    , deleteKeyValueEntry = deleteEntry publishAPI
    , purgeKeyValueEntry = purgeEntry publishAPI
    , watchKeyValues = createWatcher messageAPI
    , fetchKeyValueWatch = fetchWatcher
    , stopKeyValueWatch = stopWatcher
    , listKeyValueKeys = listKeys messageAPI
    , getKeyValueHistory = entryHistory messageAPI
    , purgeDeletedKeyValueEntries = purgeDeletedEntries streamAPI messageAPI
    }

createBucket
  :: Stream.StreamAPI
  -> KeyValueBucketName
  -> [KeyValueConfigOption]
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueBucket)
createBucket streamAPI bucket options requestOptions =
  case validateKeyValueConfig config of
    Left err -> pure (Left err)
    Right () -> do
      created <- Stream.create streamAPI streamName [subject]
        (keyValueStreamOptions config) requestOptions
      case created of
        Right info ->
          pure (validateBucketInfo handle info >> Right handle)
        Left err
          | isApiError 10058 err -> existingBucket
          | otherwise -> pure (Left (mapBucketError bucket err))
  where
    config = keyValueConfig bucket options
    handle = KeyValueBucket bucket
    streamName = keyValueStreamName bucket
    subject = keyValuePatternSubject bucket ">"
    existingBucket = do
      existing <- Stream.info streamAPI streamName requestOptions
      pure $ do
        info <- either (Left . mapBucketError bucket) Right existing
        validateBucketInfo handle info
        if compatibleBucketConfig config (streamInfoConfig info)
          then Right handle
          else Left (KeyValueBucketExists bucket)

configureBucket
  :: ( BS.ByteString
    -> [BS.ByteString]
    -> [Stream.StreamConfigOption]
    -> [JetStreamRequestOption]
    -> IO (Either JetStreamError StreamInfo)
     )
  -> KeyValueBucketName
  -> [KeyValueConfigOption]
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueBucket)
configureBucket configure bucket options requestOptions =
  case validateKeyValueConfig config of
    Left err -> pure (Left err)
    Right () -> do
      result <- configure streamName [subject]
        (keyValueStreamOptions config) requestOptions
      pure $ do
        info <- either (Left . mapBucketError bucket) Right result
        validateBucketInfo handle info
        Right handle
  where
    config = keyValueConfig bucket options
    handle = KeyValueBucket bucket
    streamName = keyValueStreamName bucket
    subject = keyValuePatternSubject bucket ">"

lookupBucket
  :: Stream.StreamAPI
  -> KeyValueBucketName
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueBucket)
lookupBucket streamAPI bucket requestOptions =
  case validateKeyValueBucketName bucket of
    Left err -> pure (Left err)
    Right () -> do
      info <- Stream.info streamAPI (keyValueStreamName bucket) requestOptions
      pure $ do
        detail <- either (Left . mapBucketError bucket) Right info
        let handle = KeyValueBucket bucket
        validateBucketInfo handle detail
        Right handle

deleteBucket
  :: Stream.StreamAPI
  -> KeyValueBucketName
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError ())
deleteBucket streamAPI bucket requestOptions =
  case validateKeyValueBucketName bucket of
    Left err -> pure (Left err)
    Right () -> do
      deleted <- Stream.delete streamAPI (keyValueStreamName bucket) requestOptions
      pure $ do
        response <- either (Left . mapBucketError bucket) Right deleted
        if Stream.deleteStreamSuccess response
          then Right ()
          else Left (KeyValueDecodeError "JetStream did not delete key-value bucket")

listBuckets
  :: Stream.StreamAPI
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError [KeyValueBucket])
listBuckets streamAPI requestOptions =
  fmap (fmap (map KeyValueBucket . mapMaybe bucketFromStreamName))
    (listAllKeyValueStreamNames streamAPI requestOptions)

listStatuses
  :: Stream.StreamAPI
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError [KeyValueStatus])
listStatuses streamAPI requestOptions =
  go 0 []
  where
    go offset reversedStatuses = do
      response <- Stream.list streamAPI
        [ Stream.withStreamListOffset offset
        , Stream.withStreamListSubject "$KV.*.>"
        ]
        requestOptions
      case response of
        Left err -> pure (Left (KeyValueJetStreamError err))
        Right page ->
          case catMaybes <$> traverse listedStatus (streamListStreams page) of
            Left err -> pure (Left err)
            Right pageStatuses -> do
              let accumulated = reverse pageStatuses ++ reversedStatuses
                  nextOffset = streamListOffset page + length (streamListStreams page)
              if null (streamListStreams page) || nextOffset >= streamListTotal page
                then pure (Right (reverse accumulated))
                else go nextOffset accumulated

    listedStatus info =
      case bucketFromStreamName (streamConfigName (streamInfoConfig info)) of
        Nothing -> Right Nothing
        Just bucketName -> do
          let bucket = KeyValueBucket bucketName
          validateBucketInfo bucket info
          Just <$> statusFromInfo bucket info

bucketStatus
  :: Stream.StreamAPI
  -> KeyValueBucket
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueStatus)
bucketStatus streamAPI bucket requestOptions = do
  info <- Stream.info streamAPI
    (keyValueStreamName (keyValueBucketName bucket)) requestOptions
  pure $ do
    detail <- either (Left . mapBucketError (keyValueBucketName bucket)) Right info
    validateBucketInfo bucket detail
    statusFromInfo bucket detail

getLatestEntry
  :: Stream.StreamAPI
  -> KeyValueBucket
  -> KeyValueKey
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueEntry)
getLatestEntry streamAPI bucket key requestOptions = do
  result <- getRawEntry streamAPI bucket key
    (LastStreamMessageForSubject (keyValueSubject bucketName key)) requestOptions
  pure (result >>= requireValueEntry bucket key)
  where
    bucketName = keyValueBucketName bucket

getEntryRevision
  :: Stream.StreamAPI
  -> KeyValueBucket
  -> KeyValueKey
  -> KeyValueRevision
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueEntry)
getEntryRevision streamAPI bucket key revision requestOptions = do
  result <- getRawEntry streamAPI bucket key selector requestOptions
  pure (result >>= requireValueEntry bucket key)
  where
    selector
      | revision == 0 = LastStreamMessageForSubject
          (keyValueSubject (keyValueBucketName bucket) key)
      | otherwise = StreamMessageBySequence revision

getRawEntry
  :: Stream.StreamAPI
  -> KeyValueBucket
  -> KeyValueKey
  -> StreamMessageSelector
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueEntry)
getRawEntry streamAPI bucket key selector requestOptions =
  case validateKeyValueKey key of
    Left err -> pure (Left err)
    Right () -> do
      message <- Stream.getMessage streamAPI
        (keyValueStreamName bucketName) selector requestOptions
      pure $ do
        stored <- either (Left . mapEntryReadError bucketName key) Right message
        keyValueEntryFromStreamMessage bucket key stored
  where
    bucketName = keyValueBucketName bucket

requireValueEntry
  :: KeyValueBucket
  -> KeyValueKey
  -> KeyValueEntry
  -> Either KeyValueError KeyValueEntry
requireValueEntry bucket key entry =
  case keyValueEntryOperation entry of
    KeyValuePut -> Right entry
    _           -> Left (KeyValueKeyNotFound (keyValueBucketName bucket) key)

putEntry
  :: Publish.PublishAPI
  -> KeyValueBucket
  -> KeyValueKey
  -> KeyValueValue
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueRevision)
putEntry publishAPI bucket key value =
  publishEntry publishAPI bucket key value Nothing []

createEntry
  :: Stream.StreamAPI
  -> Publish.PublishAPI
  -> KeyValueBucket
  -> KeyValueKey
  -> KeyValueValue
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueRevision)
createEntry streamAPI publishAPI bucket key value requestOptions = do
  created <- updateEntry publishAPI bucket key value 0 requestOptions
  case created of
    Right revision -> pure (Right revision)
    Left (KeyValueRevisionMismatch {}) -> do
      existing <- getRawEntry streamAPI bucket key
        (LastStreamMessageForSubject (keyValueSubject bucketName key)) requestOptions
      case existing of
        Right entry
          | keyValueEntryOperation entry /= KeyValuePut ->
              updateEntry publishAPI bucket key value
                (keyValueEntryRevision entry) requestOptions
          | otherwise ->
              pure (Left (KeyValueKeyExists bucketName key))
        Left (KeyValueKeyNotFound _ _) ->
          updateEntry publishAPI bucket key value 0 requestOptions
            >>= \retry -> pure $
              case retry of
                Left (KeyValueRevisionMismatch {}) ->
                  Left (KeyValueKeyExists bucketName key)
                other -> other
        Left err ->
          pure (Left err)
    Left err -> pure (Left err)
  where
    bucketName = keyValueBucketName bucket

updateEntry
  :: Publish.PublishAPI
  -> KeyValueBucket
  -> KeyValueKey
  -> KeyValueValue
  -> KeyValueRevision
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueRevision)
updateEntry publishAPI bucket key value revision =
  publishEntry publishAPI bucket key value (Just revision)
    [Publish.withPublishExpectation (Publish.ExpectedLastSubjectSequence revision)]

publishEntry
  :: Publish.PublishAPI
  -> KeyValueBucket
  -> KeyValueKey
  -> KeyValueValue
  -> Maybe KeyValueRevision
  -> [Publish.PublishOption]
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueRevision)
publishEntry publishAPI bucket key value expectedRevision options requestOptions =
  case validateKeyValueKey key of
    Left err -> pure (Left err)
    Right () -> do
      published <- Publish.publish publishAPI
        (keyValueSubject bucketName key)
        value
        options
        requestOptions
      pure $ either
        (Left . mapEntryWriteError bucketName key (fromMaybe 0 expectedRevision))
        (Right . Publish.publishAckSequence)
        published
  where
    bucketName = keyValueBucketName bucket

deleteEntry
  :: Publish.PublishAPI
  -> KeyValueBucket
  -> KeyValueKey
  -> [KeyValueDeleteOption]
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueRevision)
deleteEntry publishAPI =
  publishDeleteMarker publishAPI KeyValueDelete

purgeEntry
  :: Publish.PublishAPI
  -> KeyValueBucket
  -> KeyValueKey
  -> [KeyValueDeleteOption]
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueRevision)
purgeEntry publishAPI =
  publishDeleteMarker publishAPI KeyValuePurge

publishDeleteMarker
  :: Publish.PublishAPI
  -> KeyValueOperation
  -> KeyValueBucket
  -> KeyValueKey
  -> [KeyValueDeleteOption]
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueRevision)
publishDeleteMarker publishAPI operation bucket key options =
  publishEntry publishAPI bucket key BS.empty
    (keyValueDeleteExpectedRevision config) publishOptions
  where
    config = keyValueDeleteConfig options
    operationHeaders =
      case operation of
        KeyValueDelete -> [("KV-Operation", "DEL")]
        KeyValuePurge ->
          [ ("KV-Operation", "PURGE")
          , ("Nats-Rollup", "sub")
          ]
        KeyValuePut -> []
    publishOptions =
      Publish.withHeaders operationHeaders
        : maybe []
          (\revision ->
            [Publish.withPublishExpectation
              (Publish.ExpectedLastSubjectSequence revision)])
          (keyValueDeleteExpectedRevision config)

createWatcher
  :: Message.MessageAPI
  -> KeyValueBucket
  -> [KeyValuePattern]
  -> [KeyValueWatchOption]
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueWatcher)
createWatcher messageAPI bucket patterns options requestOptions =
  case validateWatcherInputs actualPatterns config of
    Left err -> pure (Left err)
    Right () -> do
      created <- Message.createOrderedConsumer messageAPI streamName
        consumerOptions requestOptions
      case created of
        Left err -> pure (Left (mapBucketError bucketName err))
        Right consumer -> do
          info <- Message.orderedConsumerInfo consumer requestOptions
          case info of
            Left err -> do
              void (Message.stopOrderedConsumer consumer requestOptions)
              pure (Left (mapBucketError bucketName err))
            Right detail -> do
              let initialPending
                    | keyValueWatchUpdatesOnly config = 0
                    | otherwise = consumerInfoNumPending detail
              remaining <- newTVarIO initialPending
              complete <- newTVarIO (initialPending == 0)
              pure (Right KeyValueWatcher
                { keyValueWatcherBucket = bucket
                , keyValueWatcherConsumer = consumer
                , keyValueWatcherIgnoreDeletes = keyValueWatchIgnoreDeletes config
                , keyValueWatcherInitialRemaining = remaining
                , keyValueWatcherInitialComplete = complete
                })
  where
    bucketName = keyValueBucketName bucket
    streamName = keyValueStreamName bucketName
    actualPatterns
      | null patterns = [">"]
      | otherwise = patterns
    config = keyValueWatchConfig options
    filters = map (keyValuePatternSubject bucketName) actualPatterns
    consumerFilter =
      case filters of
        [subject] -> ConsumerFilterSubject subject
        subjects  -> ConsumerFilterSubjects subjects
    deliverPolicy
      | keyValueWatchUpdatesOnly config = DeliverNew
      | keyValueWatchIncludeHistory config = DeliverAll
      | otherwise = DeliverLastPerSubject
    consumerOptions =
      [ withOrderedConsumerDeliverPolicy deliverPolicy
      , withOrderedConsumerFilter consumerFilter
      ] ++
      [ withOrderedConsumerHeadersOnly True
      | keyValueWatchMetadataOnly config
      ]

validateWatcherInputs
  :: [KeyValuePattern]
  -> KeyValueWatchConfig
  -> Either KeyValueError ()
validateWatcherInputs patterns config = do
  mapM_ validateKeyValuePattern patterns
  validateKeyValueWatchConfig config

fetchWatcher
  :: KeyValueWatcher
  -> [Message.FetchOption]
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError KeyValueWatchBatch)
fetchWatcher watcher options requestOptions = do
  response <- Message.fetchOrdered
    (keyValueWatcherConsumer watcher) options requestOptions
  case response of
    Left err -> pure (Left (KeyValueJetStreamError err))
    Right result -> do
      let rawMessages = pullResponseMessages result
      case traverse
          (keyValueEntryFromMessage (keyValueWatcherBucket watcher))
          rawMessages of
        Left err -> pure (Left err)
        Right entries -> do
          updateInitialProgress watcher entries
          complete <- readTVarIO (keyValueWatcherInitialComplete watcher)
          pure (Right KeyValueWatchBatch
            { keyValueWatchEntries = filterWatcherEntries watcher entries
            , keyValueWatchInitialComplete = complete
            , keyValueWatchStatus = pullResponseStatus result
            })

updateInitialProgress :: KeyValueWatcher -> [KeyValueEntry] -> IO ()
updateInitialProgress watcher entries =
  atomically $ do
    complete <- readTVar (keyValueWatcherInitialComplete watcher)
    unless complete $ do
      modifyTVar' (keyValueWatcherInitialRemaining watcher)
        (max 0 . subtract received)
      remaining <- readTVar (keyValueWatcherInitialRemaining watcher)
      when (remaining == 0 || finalPending == Just 0) $
        writeTVar (keyValueWatcherInitialComplete watcher) True
  where
    received = toInteger (length entries)
    finalPending = foldl'
      (\_ entry -> Just (keyValueEntryDelta entry)) Nothing entries

filterWatcherEntries :: KeyValueWatcher -> [KeyValueEntry] -> [KeyValueEntry]
filterWatcherEntries watcher
  | keyValueWatcherIgnoreDeletes watcher =
      filter ((== KeyValuePut) . keyValueEntryOperation)
  | otherwise = id

stopWatcher
  :: KeyValueWatcher
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError ())
stopWatcher watcher requestOptions =
  fmap (either (Left . KeyValueJetStreamError) Right)
    (Message.stopOrderedConsumer
      (keyValueWatcherConsumer watcher) requestOptions)

listKeys
  :: Message.MessageAPI
  -> KeyValueBucket
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError [KeyValueKey])
listKeys messageAPI bucket requestOptions =
  withInitialEntries messageAPI bucket []
    [withKeyValueIgnoreDeletes, withKeyValueMetadataOnly]
    requestOptions $ \entries ->
      let keys = compact (sort (map keyValueEntryKey entries))
      in if null keys
        then Left (KeyValueNoKeysFound (keyValueBucketName bucket))
        else Right keys

entryHistory
  :: Message.MessageAPI
  -> KeyValueBucket
  -> KeyValueKey
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError [KeyValueEntry])
entryHistory messageAPI bucket key requestOptions =
  case validateKeyValueKey key of
    Left err -> pure (Left err)
    Right () ->
      withInitialEntries messageAPI bucket [key]
        [withKeyValueIncludeHistory] requestOptions $ \entries ->
          if null entries
            then Left (KeyValueKeyNotFound (keyValueBucketName bucket) key)
            else Right entries

withInitialEntries
  :: Message.MessageAPI
  -> KeyValueBucket
  -> [KeyValuePattern]
  -> [KeyValueWatchOption]
  -> [JetStreamRequestOption]
  -> ([KeyValueEntry] -> Either KeyValueError value)
  -> IO (Either KeyValueError value)
withInitialEntries messageAPI bucket patterns options requestOptions use = do
  watcherResult <- createWatcher messageAPI bucket patterns options requestOptions
  case watcherResult of
    Left err -> pure (Left err)
    Right watcher ->
      fmap (>>= use)
        (collectInitialEntries watcher requestOptions)
        `finally` void (stopWatcher watcher requestOptions)

collectInitialEntries
  :: KeyValueWatcher
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError [KeyValueEntry])
collectInitialEntries watcher requestOptions =
  go []
  where
    go reversedEntries = do
      batch <- fetchWatcher watcher
        [withFetchBatch 256, withFetchWait (FetchNoWaitMicros 100000)]
        requestOptions
      case batch of
        Left err -> pure (Left err)
        Right result -> do
          let accumulated =
                reverse (keyValueWatchEntries result) ++ reversedEntries
          if keyValueWatchInitialComplete result
            then pure (Right (reverse accumulated))
            else case keyValueWatchStatus result of
              Nothing -> go accumulated
              Just _ -> pure (Left
                (KeyValueDecodeError "key-value watcher ended before initial values arrived"))

purgeDeletedEntries
  :: Stream.StreamAPI
  -> Message.MessageAPI
  -> KeyValueBucket
  -> [KeyValuePurgeDeletesOption]
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError ())
purgeDeletedEntries streamAPI messageAPI bucket options requestOptions = do
  entriesResult <- withInitialEntries messageAPI bucket []
    [withKeyValueMetadataOnly] requestOptions Right
  case entriesResult of
    Left err -> pure (Left err)
    Right entries -> do
      now <- getCurrentTime
      foldM (purgeMarker now) (Right ()) (filter isDeleteMarker entries)
  where
    configuredThreshold =
      keyValueDeleteMarkersOlderThan (keyValuePurgeDeletesConfig options)
    threshold
      | configuredThreshold == 0 = 1800
      | otherwise = configuredThreshold
    isDeleteMarker entry =
      keyValueEntryOperation entry /= KeyValuePut
    purgeMarker _ (Left err) _ = pure (Left err)
    purgeMarker now (Right ()) entry = do
      let age = diffUTCTime now (keyValueEntryCreated entry)
          keep
            | threshold > 0 && age < threshold = 1
            | otherwise = 0
      purged <- Stream.purge streamAPI
        (keyValueStreamName (keyValueBucketName bucket))
        [ Stream.withPurgeSubject
            (keyValueSubject (keyValueBucketName bucket) (keyValueEntryKey entry))
        , Stream.withPurgeKeep keep
        ]
        requestOptions
      pure $ do
        response <- either (Left . KeyValueJetStreamError) Right purged
        if Stream.purgeStreamSuccess response
          then Right ()
          else Left (KeyValueDecodeError "JetStream did not purge key-value tombstone")

listAllKeyValueStreamNames
  :: Stream.StreamAPI
  -> [JetStreamRequestOption]
  -> IO (Either KeyValueError [BS.ByteString])
listAllKeyValueStreamNames streamAPI requestOptions =
  go 0 []
  where
    go offset reversedNames = do
      response <- Stream.names streamAPI
        [ Stream.withStreamListOffset offset
        , Stream.withStreamListSubject "$KV.*.>"
        ]
        requestOptions
      case response of
        Left err -> pure (Left (KeyValueJetStreamError err))
        Right page -> do
          let pageNames = streamNamesStreams page
              accumulated = reverse pageNames ++ reversedNames
              nextOffset = streamNamesOffset page + length pageNames
          if null pageNames || nextOffset >= streamNamesTotal page
            then pure (Right (reverse accumulated))
            else go nextOffset accumulated

bucketFromStreamName :: BS.ByteString -> Maybe KeyValueBucketName
bucketFromStreamName streamName =
  if "KV_" `BS.isPrefixOf` streamName
    then let bucket = BS.drop 3 streamName
      in case validateKeyValueBucketName bucket of
        Right () -> Just bucket
        Left _   -> Nothing
    else Nothing

validateBucketInfo
  :: KeyValueBucket
  -> StreamInfo
  -> Either KeyValueError ()
validateBucketInfo bucket info
  | streamConfigMaxMessagesPerSubject (streamInfoConfig info) < 1 =
      Left (KeyValueInvalidBucket (keyValueBucketName bucket))
  | otherwise = Right ()

statusFromInfo
  :: KeyValueBucket
  -> StreamInfo
  -> Either KeyValueError KeyValueStatus
statusFromInfo bucket info
  | history > toInteger (maxBound :: Int) =
      Left (KeyValueInvalidBucket bucketName)
  | otherwise =
      Right KeyValueStatus
        { keyValueStatusBucket = bucketName
        , keyValueStatusValues = streamStateMessages state
        , keyValueStatusBytes = streamStateBytes state
        , keyValueStatusConfig = KeyValueConfig
            { keyValueConfigBucket = bucketName
            , keyValueConfigDescription = streamConfigDescription config
            , keyValueConfigMaxValueSize = streamConfigMaxMessageSize config
            , keyValueConfigHistory = fromInteger history
            , keyValueConfigTTL = streamConfigMaxAge config
            , keyValueConfigMaxBytes = streamConfigMaxBytes config
            , keyValueConfigStorage = streamConfigStorage config
            , keyValueConfigReplicas = streamConfigReplicas config
            , keyValueConfigCompression = streamConfigCompression config == S2Compression
            }
        }
  where
    bucketName = keyValueBucketName bucket
    config = streamInfoConfig info
    state = streamInfoState info
    history = streamConfigMaxMessagesPerSubject config

compatibleBucketConfig :: KeyValueConfig -> StreamConfig -> Bool
compatibleBucketConfig expected actual =
  normalizeDescription (keyValueConfigDescription expected)
      == normalizeDescription (streamConfigDescription actual)
    && streamConfigSubjects actual
      == Just [keyValuePatternSubject (keyValueConfigBucket expected) ">"]
    && streamConfigRetention actual == LimitsPolicy
    && streamConfigDiscard actual == DiscardNew
    && streamConfigMaxConsumers actual == (-1)
    && streamConfigMaxMessages actual == (-1)
    && streamConfigMaxMessagesPerSubject actual
      == toInteger (keyValueConfigHistory expected)
    && streamConfigMaxBytes actual == keyValueConfigMaxBytes expected
    && streamConfigMaxAge actual == keyValueConfigTTL expected
    && streamConfigMaxMessageSize actual == keyValueConfigMaxValueSize expected
    && streamConfigStorage actual == keyValueConfigStorage expected
    && streamConfigReplicas actual == keyValueConfigReplicas expected
    && streamConfigDuplicateWindow actual
      == Just (keyValueDuplicateWindow expected)
    && streamConfigDenyDelete actual
    && streamConfigAllowRollup actual
    && streamConfigAllowDirect actual
    && (streamConfigCompression actual == S2Compression)
      == keyValueConfigCompression expected
  where
    normalizeDescription = fromMaybe BS.empty

mapBucketError :: KeyValueBucketName -> JetStreamError -> KeyValueError
mapBucketError bucket err
  | isApiError 10059 err = KeyValueBucketNotFound bucket
  | otherwise = KeyValueJetStreamError err

mapEntryReadError
  :: KeyValueBucketName
  -> KeyValueKey
  -> JetStreamError
  -> KeyValueError
mapEntryReadError bucket key err
  | isApiError 10037 err = KeyValueKeyNotFound bucket key
  | isApiError 10059 err = KeyValueBucketNotFound bucket
  | otherwise = KeyValueJetStreamError err

mapEntryWriteError
  :: KeyValueBucketName
  -> KeyValueKey
  -> KeyValueRevision
  -> JetStreamError
  -> KeyValueError
mapEntryWriteError bucket key revision err
  | isApiError 10071 err = KeyValueRevisionMismatch bucket key revision
  | isApiError 10059 err = KeyValueBucketNotFound bucket
  | otherwise = KeyValueJetStreamError err

isApiError :: Int -> JetStreamError -> Bool
isApiError code (JetStreamApiFailure err) =
  apiErrorCodeDetail err == code
isApiError _ _ = False

compact :: Eq value => [value] -> [value]
compact [] = []
compact (value:values) =
  value : compact (dropWhile (== value) values)
