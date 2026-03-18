{-# LANGUAGE OverloadedStrings #-}

module TestSupport
  ( Endpoints (..)
  , clientSystemTest
  , fixturePath
  , readFixtureBytesRaw
  , readFixtureBytesTrim
  , readFixtureText
  , systemTest
  , testLoggerOptions
  , withNatsContainer
  , withNatsContainerConfig
  , withNatsContainerConfigNamed
  , withNatsContainerConfigWithMounts
  , withNatsContainerConfigWithMountsNamed
  ) where

import           Client
import           Control.Concurrent     (threadDelay)
import           Control.Exception      (bracket, onException)
import           Control.Monad          (filterM)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BC
import           Data.Char              (isSpace)
import           Data.List              (dropWhileEnd)
import           Data.String            (fromString)
import           NatsServerConfig
import           System.Directory
    ( canonicalizePath
    , doesDirectoryExist
    , getCurrentDirectory
    , getTemporaryDirectory
    , removeFile
    )
import           System.IO
    ( hClose
    , hPutStrLn
    , openBinaryTempFile
    , stderr
    )
import           System.Timeout         (timeout)
import           Test.Hspec
import           TestContainers         hiding (stderr)
import qualified TestContainers.Hspec   as TC

data Endpoints = Endpoints
                   { natsHost :: String
                   , natsPort :: Int
                   }

fileLogConsumer :: FilePath -> LogConsumer
fileLogConsumer fp pipe line =
  case pipe of
    Stdout -> do
      BS.appendFile fp line
      BS.appendFile fp "\r\n"
    Stderr -> do
      BS.appendFile fp line
      BS.appendFile fp "\r\n"

testLoggerOptions :: [ConfigOption]
testLoggerOptions =
  [ withMinimumLogLevel Debug
  , withLogAction (putStrLn . renderLogEntry)
  ]

testTimeoutMicros :: Int
testTimeoutMicros = 20 * 1000000

withTestTimeoutMicros :: Int -> IO () -> IO ()
withTestTimeoutMicros timeoutMicros action = do
  result <- timeout timeoutMicros action
  case result of
    Nothing -> expectationFailure "test timed out"
    Just () -> pure ()

systemTest :: Spec -> Spec
systemTest =
  around_ (withTestTimeoutMicros testTimeoutMicros)

clientSystemTest :: String -> String -> ([ConfigOption] -> Endpoints -> IO ()) -> Spec
clientSystemTest testId label action =
  systemTest . describe "client" $ do
      around (withNatsContainerConfigNamed testId defaultNatsConfigOptions) $ do
        it label $ \endpoints -> action testLoggerOptions endpoints

trimString :: String -> String
trimString =
  dropWhileEnd isSpace . dropWhile isSpace

fixturesDir :: IO FilePath
fixturesDir = do
  cwd <- getCurrentDirectory
  let candidates = [cwd ++ "/system-tests/fixtures", cwd ++ "/fixtures"]
  existing <- filterM doesDirectoryExist candidates
  case existing of
    (dir:_) -> canonicalizePath dir
    []      -> fail "fixtures directory not found (expected system-tests/fixtures)"

fixturePath :: FilePath -> IO FilePath
fixturePath rel = do
  baseDir <- fixturesDir
  pure (baseDir ++ "/" ++ rel)

readFixtureText :: FilePath -> IO String
readFixtureText rel =
  trimString <$> (readFile =<< fixturePath rel)

readFixtureBytesTrim :: FilePath -> IO BS.ByteString
readFixtureBytesTrim rel =
  BC.pack <$> readFixtureText rel

readFixtureBytesRaw :: FilePath -> IO BS.ByteString
readFixtureBytesRaw rel =
  BS.readFile =<< fixturePath rel

defaultNatsConfigOptions :: NatsConfigOptions
defaultNatsConfigOptions =
  [WithLogVerbosity NatsLogDebug]

withNatsConfig :: NatsConfigOptions -> (FilePath -> IO a) -> IO a
withNatsConfig options =
  bracket (writeNatsServerConfigFile options) removeFile

withNatsContainer :: (Endpoints -> IO ()) -> IO ()
withNatsContainer =
  withNatsContainerConfig defaultNatsConfigOptions

withNatsContainerConfig :: NatsConfigOptions -> (Endpoints -> IO ()) -> IO ()
withNatsContainerConfig config =
  withNatsContainerConfigWithMounts config []

withNatsContainerConfigNamed :: String -> NatsConfigOptions -> (Endpoints -> IO ()) -> IO ()
withNatsContainerConfigNamed testId config =
  withNatsContainerConfigWithMountsNamed testId config []

withNatsContainerConfigWithMounts :: NatsConfigOptions -> [(FilePath, FilePath)] -> (Endpoints -> IO ()) -> IO ()
withNatsContainerConfigWithMounts config mounts action =
  withTempLogFile $ \logFile ->
    withNatsConfig config $ \configFile ->
      TC.withContainers (container configFile mounts logFile) action

withNatsContainerConfigWithMountsNamed :: String -> NatsConfigOptions -> [(FilePath, FilePath)] -> (Endpoints -> IO ()) -> IO ()
withNatsContainerConfigWithMountsNamed testId config mounts action =
  withTempLogFileNamed testId $ \logFile ->
    withNatsConfig config $ \configFile ->
      TC.withContainers (container configFile mounts logFile) action

waitForLogLineInFile :: Int -> BS.ByteString -> FilePath -> IO ()
waitForLogLineInFile timeoutMicros needle logFile = do
  result <- timeout timeoutMicros poll
  case result of
    Just () -> pure ()
    Nothing ->
      expectationFailure ("timed out waiting for NATS log line: " ++ BC.unpack needle)
  where
    poll = do
      contents <- BS.readFile logFile
      if needle `BS.isInfixOf` contents
        then pure ()
        else do
          threadDelay 100000
          poll

container :: FilePath -> [(FilePath, FilePath)] -> FilePath -> TC.TestContainer Endpoints
container configFile mounts logFile = do
  let volumeMounts =
        (fromString configFile, fromString "/etc/nats/nats-server.conf")
          : [(fromString hostPath, fromString containerPath) | (hostPath, containerPath) <- mounts]
  natsContainer <- TC.run (TC.containerRequest
    (TC.fromTag "nats:latest")
    TC.& TC.setVolumeMounts volumeMounts
    TC.& TC.setCmd ["-c", "/etc/nats/nats-server.conf"]
    TC.& TC.setExpose [4222]
    TC.& TC.setWaitingFor mempty
    TC.& withFollowLogs (fileLogConsumer logFile)
    )

  liftIO $
    waitForLogLineInFile
      testTimeoutMicros
      "Server is ready"
      logFile

  pure $ Endpoints
    { natsHost = "0.0.0.0"
    , natsPort =
        TC.containerPort natsContainer 4222
    }

withTempLogFile :: (FilePath -> IO a) -> IO a
withTempLogFile =
  withTempLogFileNamed ""

withTempLogFileNamed :: String -> (FilePath -> IO a) -> IO a
withTempLogFileNamed testId action = do
  tmpDir <- getTemporaryDirectory
  (logFile, handle) <- openBinaryTempFile tmpDir (buildLogFilePrefix testId)
  hClose handle
  let reportFailure =
        hPutStrLn stderr ("NATS system test log: " ++ logFile)
  result <- action logFile `onException` reportFailure
  removeFile logFile
  pure result

buildLogFilePrefix :: String -> String
buildLogFilePrefix testId =
  case testId of
    "" -> "natskell-system-test-"
    _  -> "natskell-system-test-" ++ testId ++ "-"
