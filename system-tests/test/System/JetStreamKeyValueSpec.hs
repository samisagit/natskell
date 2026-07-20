{-# LANGUAGE OverloadedStrings #-}

module JetStreamKeyValueSpec (spec) where

import qualified API               as Nats
import           Client            (withConnectName)
import           Control.Exception (finally)
import qualified Data.ByteString   as BS
import           Data.List         (sort)
import qualified JetStream.API     as JetStream
import           JetStream.Client  (newJetStream)
import           NatsServerConfig
import           Test.Hspec
import           TestSupport

spec :: Spec
spec =
  systemTest $ do
    describe "jetstream key-value" $ do
      around (withNatsContainerConfigNamed testId serverOptions) $ do
        it "implements bucket, CAS, history, watch, and purge semantics" $ \endpoints ->
          withJetStream endpoints scenario

testId :: String
testId = "7c4e055b-2168-4fba-9cea-2607c8b91ad2"

serverOptions :: NatsConfigOptions
serverOptions =
  [ WithLogVerbosity NatsLogDebug
  , WithJetStream
  ]

withJetStream :: Endpoints -> (JetStream.JetStream -> IO ()) -> IO ()
withJetStream (Endpoints host port) action = do
  client <- newTestClient [(host, port)] $
    withConnectName "natskell-key-value-system-test" : testLoggerOptions
  jetStream <- either (fail . show) pure (newJetStream client [])
  action jetStream `finally` Nats.close client []

scenario :: JetStream.JetStream -> IO ()
scenario jetStream = do
  let api = JetStream.keyValues jetStream
      bucketName = "NATSKELL_KV"
      configOptions =
        [ JetStream.withKeyValueDescription "system test bucket"
        , JetStream.withKeyValueHistory 3
        , JetStream.withKeyValueMaxValueSize 1024
        , JetStream.withKeyValueMaxBytes 1048576
        , JetStream.withKeyValueStorage JetStream.MemoryStorage
        ]

  bucket <- expectRight "create bucket" $
    JetStream.createKeyValueBucket api bucketName configOptions []
  JetStream.keyValueBucketName bucket `shouldBe` bucketName

  duplicate <- expectRight "create existing compatible bucket" $
    JetStream.createKeyValueBucket api bucketName configOptions []
  duplicate `shouldBe` bucket
  JetStream.createKeyValueBucket api bucketName
    [JetStream.withKeyValueHistory 2] []
    `shouldReturn` Left (JetStream.KeyValueBucketExists bucketName)

  updatedBucket <- expectRight "update bucket" $
    JetStream.updateKeyValueBucket api bucketName
      (configOptions ++ [JetStream.withKeyValueHistory 4]) []
  updatedStatus <- expectRight "updated bucket status" $
    JetStream.getKeyValueStatus api updatedBucket []
  JetStream.keyValueConfigHistory (JetStream.keyValueStatusConfig updatedStatus)
    `shouldBe` 4
  _ <- expectRight "restore bucket configuration" $
    JetStream.updateKeyValueBucket api bucketName configOptions []

  upserted <- expectRight "create-or-update bucket" $
    JetStream.createOrUpdateKeyValueBucket api "NATSKELL_UPSERT"
      [JetStream.withKeyValueStorage JetStream.MemoryStorage] []
  _ <- expectRight "create-or-update existing bucket" $
    JetStream.createOrUpdateKeyValueBucket api "NATSKELL_UPSERT"
      [ JetStream.withKeyValueDescription "updated"
      , JetStream.withKeyValueStorage JetStream.MemoryStorage
      ] []
  upsertedStatus <- expectRight "create-or-update status" $
    JetStream.getKeyValueStatus api upserted []
  JetStream.keyValueConfigDescription (JetStream.keyValueStatusConfig upsertedStatus)
    `shouldBe` Just "updated"
  JetStream.deleteKeyValueBucket api "NATSKELL_UPSERT" [] `shouldReturn` Right ()

  status <- expectRight "bucket status" $
    JetStream.getKeyValueStatus api bucket []
  JetStream.keyValueStatusValues status `shouldBe` 0
  JetStream.keyValueStatusBytes status `shouldBe` 0
  let statusConfig = JetStream.keyValueStatusConfig status
  JetStream.keyValueConfigDescription statusConfig `shouldBe` Just "system test bucket"
  JetStream.keyValueConfigHistory statusConfig `shouldBe` 3
  JetStream.keyValueConfigMaxValueSize statusConfig `shouldBe` 1024
  JetStream.keyValueConfigStorage statusConfig `shouldBe` JetStream.MemoryStorage
  JetStream.keyValueConfigReplicas statusConfig `shouldBe` 1

  buckets <- expectRight "list buckets" $
    JetStream.listKeyValueBuckets api []
  buckets `shouldBe` [bucket]
  statuses <- expectRight "list bucket statuses" $
    JetStream.listKeyValueStatuses api []
  map JetStream.keyValueStatusBucket statuses `shouldBe` [bucketName]

  let binaryValue = BS.pack [0, 1, 255]
  firstRevision <- expectRight "put first value" $
    JetStream.putKeyValueEntry api bucket "process.one" binaryValue []
  first <- expectRight "get first value" $
    JetStream.getKeyValueEntry api bucket "process.one" []
  JetStream.keyValueEntryValue first `shouldBe` binaryValue
  JetStream.keyValueEntryRevision first `shouldBe` firstRevision
  JetStream.keyValueEntryOperation first `shouldBe` JetStream.KeyValuePut
  historical <- expectRight "get value by revision" $
    JetStream.getKeyValueEntryRevision api bucket "process.one" firstRevision []
  JetStream.keyValueEntryValue historical `shouldBe` binaryValue
  JetStream.getKeyValueEntryRevision api bucket "process.two" firstRevision []
    `shouldReturn` Left (JetStream.KeyValueKeyNotFound bucketName "process.two")

  JetStream.updateKeyValueEntry api bucket "process.one" "wrong" 999 []
    `shouldReturn` Left
      (JetStream.KeyValueRevisionMismatch bucketName "process.one" 999)
  secondRevision <- expectRight "CAS update" $
    JetStream.updateKeyValueEntry api bucket "process.one" "v2" firstRevision []
  JetStream.createKeyValueEntry api bucket "process.one" "duplicate" []
    `shouldReturn` Left (JetStream.KeyValueKeyExists bucketName "process.one")

  _ <- expectRight "delete with CAS" $
    JetStream.deleteKeyValueEntry api bucket "process.one"
      [JetStream.withKeyValueLastRevision secondRevision] []
  JetStream.getKeyValueEntry api bucket "process.one" []
    `shouldReturn` Left (JetStream.KeyValueKeyNotFound bucketName "process.one")
  thirdRevision <- expectRight "create after tombstone" $
    JetStream.createKeyValueEntry api bucket "process.one" "v3" []
  otherRevision <- expectRight "put second key" $
    JetStream.putKeyValueEntry api bucket "process.two" "other-1" []

  watcher <- expectRight "create initial watcher" $
    JetStream.watchKeyValues api bucket [] [] []
  initial <- expectRight "fetch initial watcher values" $
    JetStream.fetchKeyValueWatch api watcher
      [ JetStream.withFetchBatch 10
      , JetStream.withFetchWait (JetStream.FetchNoWaitMicros 100000)
      ] []
  JetStream.keyValueWatchInitialComplete initial `shouldBe` True
  sort (map JetStream.keyValueEntryKey (JetStream.keyValueWatchEntries initial))
    `shouldBe` ["process.one", "process.two"]
  JetStream.stopKeyValueWatch api watcher [] `shouldReturn` Right ()

  history <- expectRight "read bounded history" $
    JetStream.getKeyValueHistory api bucket "process.one" []
  map JetStream.keyValueEntryOperation history
    `shouldBe` [JetStream.KeyValuePut, JetStream.KeyValueDelete, JetStream.KeyValuePut]
  map JetStream.keyValueEntryValue history `shouldBe` ["v2", "", "v3"]

  updates <- expectRight "create updates-only watcher" $
    JetStream.watchKeyValues api bucket ["process.two"]
      [JetStream.withKeyValueUpdatesOnly] []
  updatedRevision <- expectRight "publish watched update" $
    JetStream.updateKeyValueEntry api bucket "process.two" "other-2" otherRevision []
  updateBatch <- expectRight "fetch watched update" $
    JetStream.fetchKeyValueWatch api updates
      [ JetStream.withFetchBatch 5
      , JetStream.withFetchWait (JetStream.FetchNoWaitMicros 100000)
      ] []
  JetStream.keyValueWatchInitialComplete updateBatch `shouldBe` True
  case JetStream.keyValueWatchEntries updateBatch of
    [entry] -> do
      JetStream.keyValueEntryKey entry `shouldBe` "process.two"
      JetStream.keyValueEntryValue entry `shouldBe` "other-2"
      JetStream.keyValueEntryRevision entry `shouldBe` updatedRevision
    entries -> expectationFailure
      ("expected one watched key-value update, got " ++ show (length entries))
  JetStream.stopKeyValueWatch api updates [] `shouldReturn` Right ()

  _ <- expectRight "purge key with CAS" $
    JetStream.purgeKeyValueEntry api bucket "process.one"
      [JetStream.withKeyValueLastRevision thirdRevision] []
  purgedHistory <- expectRight "read purge marker" $
    JetStream.getKeyValueHistory api bucket "process.one" []
  map JetStream.keyValueEntryOperation purgedHistory
    `shouldBe` [JetStream.KeyValuePurge]

  keys <- expectRight "list live keys" $
    JetStream.listKeyValueKeys api bucket []
  keys `shouldBe` ["process.two"]

  JetStream.purgeDeletedKeyValueEntries api bucket
    [JetStream.withKeyValueDeleteMarkersOlderThan (-1)] []
    `shouldReturn` Right ()
  JetStream.getKeyValueHistory api bucket "process.one" []
    `shouldReturn` Left (JetStream.KeyValueKeyNotFound bucketName "process.one")

  JetStream.deleteKeyValueBucket api bucketName [] `shouldReturn` Right ()
  JetStream.lookupKeyValueBucket api bucketName []
    `shouldReturn` Left (JetStream.KeyValueBucketNotFound bucketName)

expectRight :: Show error => String -> IO (Either error value) -> IO value
expectRight label action = do
  result <- action
  case result of
    Left err    -> expectationFailure (label ++ " failed: " ++ show err) >> fail label
    Right value -> pure value
