{-# LANGUAGE OverloadedStrings #-}

module TestSupport
  ( Endpoints (..)
  , clientSystemTest
  , fixturePath
  , readFixtureBytesRaw
  , readFixtureBytesTrim
  , readFixtureText
  , systemTest
  , testLoggerConfig
  , withNatsContainer
  , withNatsContainerConfig
  , withNatsContainerConfigWithMounts
  ) where

import           Client
import           Control.Concurrent.STM
import           Control.Exception      (bracket)
import           Control.Monad          (filterM)
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
    , removeFile
    )
import           System.Timeout         (timeout)
import           Test.Hspec
import           TestContainers
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

breakLogFile :: FilePath -> IO ()
breakLogFile fp = do
  BS.appendFile fp "\r\n"
  BS.appendFile fp "\r\n"
  BS.appendFile fp "--------------------"
  BS.appendFile fp "\r\n"
  BS.appendFile fp "\r\n"

testLoggerConfig :: IO LoggerConfig
testLoggerConfig = do
  lock <- newTMVarIO ()
  pure $ LoggerConfig Debug (putStrLn . renderLogEntry) lock

testTimeoutMicros :: Int
testTimeoutMicros = 10 * 1000000

withTestTimeout :: IO () -> IO ()
withTestTimeout action = do
  result <- timeout testTimeoutMicros action
  case result of
    Nothing -> expectationFailure "test timed out after 10s"
    Just () -> pure ()

systemTest :: Spec -> Spec
systemTest =
  parallel
    . around_ withTestTimeout
    . around_ (\action -> action >> breakLogFile "nats.log")

clientSystemTest :: String -> (LoggerConfig -> Endpoints -> IO ()) -> Spec
clientSystemTest label action =
  systemTest . describe "client" $ do
      logger <- runIO testLoggerConfig
      around withNatsContainer $ do
        it label $ \endpoints -> action logger endpoints

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

defaultNatsOptions :: NatsConfigOptions
defaultNatsOptions =
  [WithLogVerbosity NatsLogDebug]

withNatsConfig :: NatsConfigOptions -> (FilePath -> IO a) -> IO a
withNatsConfig options =
  bracket (writeNatsServerConfigFile options) removeFile

withNatsContainer :: (Endpoints -> IO ()) -> IO ()
withNatsContainer =
  withNatsContainerConfig defaultNatsOptions

withNatsContainerConfig :: NatsConfigOptions -> (Endpoints -> IO ()) -> IO ()
withNatsContainerConfig config =
  withNatsContainerConfigWithMounts config []

withNatsContainerConfigWithMounts :: NatsConfigOptions -> [(FilePath, FilePath)] -> (Endpoints -> IO ()) -> IO ()
withNatsContainerConfigWithMounts config mounts action =
  withNatsConfig config $ \configFile ->
    TC.withContainers (container configFile mounts) action

container :: FilePath -> [(FilePath, FilePath)] -> TC.TestContainer Endpoints
container configFile mounts = do
  let volumeMounts =
        (fromString configFile, fromString "/etc/nats/nats-server.conf")
          : [(fromString hostPath, fromString containerPath) | (hostPath, containerPath) <- mounts]
  natsContainer <- TC.run (TC.containerRequest
    (TC.fromTag "nats:latest")
    TC.& TC.setVolumeMounts volumeMounts
    TC.& TC.setCmd ["-c", "/etc/nats/nats-server.conf"]
    TC.& TC.setExpose [4222]
    TC.& TC.setWaitingFor (TC.waitUntilMappedPortReachable 4222)
    TC.& withFollowLogs (fileLogConsumer "nats.log")
    )

  pure $ Endpoints
    { natsHost = "0.0.0.0"
    , natsPort =
        TC.containerPort natsContainer 4222
    }
