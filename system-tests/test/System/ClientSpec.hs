{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import           Client
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception      (bracket)
import           Control.Monad          (filterM, when)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BC
import           Data.Char              (isSpace)
import           Data.List              (dropWhileEnd)
import           Data.Maybe
import           Data.String            (fromString)
import           NatsServerConfig
import           System.Directory
    ( canonicalizePath
    , doesDirectoryExist
    , getCurrentDirectory
    , removeFile
    )
import           Test.Hspec
import           TestContainers         hiding (exitCode)
import qualified TestContainers.Hspec   as TC
import           WaitGroup

data Endpoints = Endpoints
                   { natsHost :: String
                   , natsPort :: Int
                   }

fileLogConsumer :: FilePath -> LogConsumer
fileLogConsumer fp pipe line = do
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

natsServerConfig :: NatsServerConfig
natsServerConfig =
  defaultNatsServerConfig
    { natsLogVerbosity = NatsLogDebug
    }

buildAuthConfig :: NatsAuthorization -> NatsServerConfig
buildAuthConfig authorization =
  defaultNatsServerConfig
    { natsLogVerbosity = NatsLogDebug
    , natsAuthorization = authorization
    }

natsUserPassConfig :: NatsServerConfig
natsUserPassConfig =
  buildAuthConfig (NatsAuthorizationUserPass "test-user" "test-pass")

natsTokenConfig :: NatsServerConfig
natsTokenConfig =
  buildAuthConfig (NatsAuthorizationToken "test-token")

natsUsersConfig :: NatsServerConfig
natsUsersConfig =
  buildAuthConfig (NatsAuthorizationUsers [NatsAuthUserPass "test-user" "test-pass"])

buildNKeyConfig :: String -> NatsServerConfig
buildNKeyConfig publicKey =
  buildAuthConfig (NatsAuthorizationNKey publicKey)

buildJwtConfig :: String -> String -> String -> NatsServerConfig
buildJwtConfig operatorJwt accountPub accountJwt =
  defaultNatsServerConfig
    { natsLogVerbosity = NatsLogDebug
    , natsJwtConfig = Just NatsJwtConfig
        { natsOperatorJwt = operatorJwt
        , natsSystemAccount = Nothing
        , natsJwtResolver = NatsJwtResolverMemory [(accountPub, accountJwt)]
        }
    }

buildTlsConfig :: FilePath -> NatsServerConfig
buildTlsConfig tlsDir =
  defaultNatsServerConfig
    { natsLogVerbosity = NatsLogDebug
    , natsAuthorization = NatsAuthorizationUsers [NatsAuthUserName "CN=natskell-test-client"]
    , natsTlsConfig = Just (buildTlsSettings tlsDir True True (Just (tlsDir ++ "/ca.crt")))
    }

buildTlsAuthConfig :: FilePath -> NatsAuthorization -> NatsServerConfig
buildTlsAuthConfig tlsDir authorization =
  (buildAuthConfig authorization)
    { natsTlsConfig = Just (buildTlsSettings tlsDir False False Nothing)
    }

buildTlsSettings :: FilePath -> Bool -> Bool -> Maybe FilePath -> NatsTlsConfig
buildTlsSettings tlsDir verify verifyAndMap caFile =
  NatsTlsConfig
    { natsTlsCertFile = tlsDir ++ "/server.crt"
    , natsTlsKeyFile = tlsDir ++ "/server.key"
    , natsTlsCaFile = caFile
    , natsTlsVerify = verify
    , natsTlsVerifyAndMap = verifyAndMap
    , natsTlsTimeout = Just 1
    }

buildJwtTlsConfig :: FilePath -> String -> String -> String -> NatsServerConfig
buildJwtTlsConfig tlsDir operatorJwt accountPub accountJwt =
  (buildJwtConfig operatorJwt accountPub accountJwt)
    { natsTlsConfig = Just (buildTlsSettings tlsDir False False Nothing)
    }

withNatsConfig :: NatsServerConfig -> (FilePath -> IO a) -> IO a
withNatsConfig config =
  bracket (writeNatsServerConfigFile config) removeFile

withNatsContainer :: (Endpoints -> IO ()) -> IO ()
withNatsContainer =
  withNatsContainerConfig natsServerConfig

withNatsContainerConfig :: NatsServerConfig -> (Endpoints -> IO ()) -> IO ()
withNatsContainerConfig config =
  withNatsContainerConfigWithMounts config []

withNatsContainerConfigWithMounts :: NatsServerConfig -> [(FilePath, FilePath)] -> (Endpoints -> IO ()) -> IO ()
withNatsContainerConfigWithMounts config mounts action =
  withNatsConfig config $ \configFile ->
    TC.withContainers (container configFile mounts) action

container :: FilePath -> [(FilePath, FilePath)] -> TC.TestContainer Endpoints
container configFile mounts = do
  let volumeMounts =
        (fromString configFile, fromString "/etc/nats/nats-server.conf")
          : [(fromString hostPath, fromString containerPath) | (hostPath, containerPath) <- mounts]
  -- Launch the container image.
  natsContainer <- TC.run (TC.containerRequest
    (TC.fromTag "nats:latest")
    -- Inject the generated config to set server verbosity.
    TC.& TC.setVolumeMounts volumeMounts
    -- Ensure the container uses the injected config file.
    TC.& TC.setCmd ["-c", "/etc/nats/nats-server.conf"]
    -- Expose the port 4222 from within the container. The respective port
    -- on the host machine can be looked up using `containerPort` (see below).
    TC.& TC.setExpose [ 4222 ]
    -- Wait until the container is ready to accept requests. `run` blocks until
    -- readiness can be established.
    TC.& TC.setWaitingFor (TC.waitUntilMappedPortReachable 4222)
    TC.& withFollowLogs (fileLogConsumer "nats.log")
    )

  pure $ Endpoints
    {
      natsHost = "0.0.0.0"
    , natsPort =
        TC.containerPort natsContainer 4222
    }

loadAuthCases = do
  nkeyPub <- readFixtureText "nkey/user.pub"
  nkeySeed <- readFixtureBytesTrim "nkey/user.seed"
  operatorJwt <- readFixtureText "jwt/operator.jwt"
  accountJwt <- readFixtureText "jwt/account.jwt"
  accountPub <- readFixtureText "jwt/account.pub"
  userCreds <- readFixtureBytesRaw "jwt/user.creds"
  clientCert <- readFixtureBytesRaw "tls/client.crt"
  clientKey <- readFixtureBytesRaw "tls/client.key"
  tlsHostDir <- fixturePath "tls"
  let tlsContainerDir = "/etc/nats/certs"
  pure
    [ ("authenticates with token", "auth-token", natsTokenConfig, [withAuthToken "test-token"], [], Nothing)
    , ("authenticates with user/pass", "auth-user-pass", natsUserPassConfig, [withUserPass ("test-user", "test-pass")], [], Nothing)
    , ("authenticates with users list", "auth-users", natsUsersConfig, [withUserPass ("test-user", "test-pass")], [], Nothing)
    , ("authenticates with nkey", "auth-nkey", buildNKeyConfig nkeyPub, [withNKey nkeySeed], [], Nothing)
    , ("authenticates with jwt creds file", "auth-jwt-creds", buildJwtConfig operatorJwt accountPub accountJwt, [withJWT userCreds], [], Nothing)
    , ("authenticates with tls and user/pass", "auth-tls-user-pass", buildTlsAuthConfig tlsContainerDir (NatsAuthorizationUserPass "test-user" "test-pass"), [withUserPass ("test-user", "test-pass"), withConnectionAttempts 1], [(tlsHostDir, tlsContainerDir)], Just 2000000)
    , ("authenticates with tls and token", "auth-tls-token", buildTlsAuthConfig tlsContainerDir (NatsAuthorizationToken "test-token"), [withAuthToken "test-token", withConnectionAttempts 1], [(tlsHostDir, tlsContainerDir)], Just 2000000)
    , ("authenticates with tls and nkey", "auth-tls-nkey", buildTlsAuthConfig tlsContainerDir (NatsAuthorizationNKey nkeyPub), [withNKey nkeySeed, withConnectionAttempts 1], [(tlsHostDir, tlsContainerDir)], Just 2000000)
    , ("authenticates with tls and jwt creds file", "auth-tls-jwt-creds", buildJwtTlsConfig tlsContainerDir operatorJwt accountPub accountJwt, [withJWT userCreds, withConnectionAttempts 1], [(tlsHostDir, tlsContainerDir)], Just 2000000)
    , ("authenticates with tls cert", "auth-tls", buildTlsConfig tlsContainerDir, [withTLSCert (clientCert, clientKey), withConnectionAttempts 1], [(tlsHostDir, tlsContainerDir)], Just 2000000)
    ]

authSpec logger (label, connectName, authConfig, authOpts, authMounts, watchdogDelay) =
  around (withNatsContainerConfigWithMounts authConfig authMounts) $ do
    it label $ \(Endpoints natsHost natsPort) -> do
      exitResult <- newEmptyTMVarIO
      pinged <- newEmptyTMVarIO
      let clientOpts =
            [ withConnectName connectName
            , withExitAction (atomically . putTMVar exitResult)
            , withLoggerConfig logger
            ] ++ authOpts
      client <- newClient [(natsHost, natsPort)] clientOpts
      forkIO $ do
        case watchdogDelay of
          Nothing -> pure ()
          Just delayUs -> do
            threadDelay delayUs
            didPut <- atomically $
              tryPutTMVar exitResult (ClientExitResult ExitStatusReset ExitResetRequested)
            when didPut (reset client)
      forkIO $ do
        outcome <- atomically $ (Left <$> readTMVar pinged) `orElse` (Right <$> readTMVar exitResult)
        case outcome of
          Left _  -> close client
          Right _ -> pure ()
      ping client (atomically (putTMVar pinged ()))
      result <- atomically $ readTMVar exitResult
      exitReason result `shouldBe` ExitClosedByUser

spec :: Spec
spec = do
  sys

sys = parallel $ do
  describe "client" $ do
    logger <- runIO testLoggerConfig
    around_ (\action -> action >> breakLogFile "nats.log") $ do
      around withNatsContainer $ do
        it "PING results in PONG"  $ \(Endpoints natsHost natsPort) -> do
          c <- newClient [(natsHost, natsPort)] [withConnectName "1f27aec6-e832-41ad-88ad-15555985b754", withLoggerConfig logger]
          wg <- newWaitGroup 1
          ping c $ done wg
          wait wg
          close c
        it "exits immediately on fatal error" $ \(Endpoints _ _) -> do
          wg <- newWaitGroup 1
          exitResult <- newEmptyTMVarIO
          _ <- newClient [("0.0.0.0", 4999)] [
            withConnectName "fatal-reset-test",
            withExitAction (\r -> atomically (putTMVar exitResult r) >> done wg),
            withLoggerConfig logger
            ]
          wait wg
          result <- atomically $ readTMVar exitResult
          exitStatus result `shouldBe` ExitStatusRetryExhausted
          exitCode result `shouldBe` 1
          case exitReason result of
            ExitRetriesExhausted _ -> pure ()
            other                  -> expectationFailure $ "Unexpected exit reason: " ++ show other
        it "user can close connection" $ \(Endpoints natsHost natsPort) -> do
          wg <- newWaitGroup 1
          exitResult <- newEmptyTMVarIO
          client <- newClient [(natsHost, natsPort)] [
            withConnectName "b9ed73e3-9674-41a2-9979-bb63b78c6579",
            withExitAction (\r -> atomically (putTMVar exitResult r) >> done wg),
            withLoggerConfig logger
            ]
          close client
          wait wg
          result <- atomically $ readTMVar exitResult
          exitReason result `shouldBe` ExitClosedByUser
          exitCode result `shouldBe` 0
        it "messages are sent and received"  $ \(Endpoints natsHost natsPort) -> do
          let topic = "SOME.TOPIC"
          let payload = "HELLO"
          lock <- newEmptyMVar
          sidBox <- newEmptyMVar
          wg <- newWaitGroup 1
          assertClient <- newClient [(natsHost, natsPort)] [withConnectName "0dfe787e-383b-4cb8-a73f-8474f4cc0497", withLoggerConfig logger]
          subscribe assertClient topic $ \msg -> do
            case msg of
              Nothing -> error "Received empty message"
              Just msg -> do
                unsubscribe assertClient (sid msg)
                putMVar lock msg
                putMVar sidBox (sid msg)
                done wg
          promptClient <- newClient [(natsHost, natsPort)] [withConnectName "0e81e61a-932f-4036-9cdd-9a65fb4ed829", withLoggerConfig logger]
          publish promptClient topic [withPayload payload]
          wait wg
          msg <- takeMVar lock
          sid' <- takeMVar sidBox
          msg `shouldBe` MsgView topic sid' Nothing (Just payload) Nothing
          close assertClient
          close promptClient
        it "replies are routed correctly"  $ \(Endpoints natsHost natsPort) -> do
          let topic = "REQ.TOPIC"
          remoteClient <- newClient [(natsHost, natsPort)] [withConnectName "6eff2527-1ad5-4b0c-b4e5-4a52a7d17639", withLoggerConfig logger]
          subscribe remoteClient topic $ \msg -> do
            case msg of
              Nothing -> error "Received empty message"
              Just msg -> do
                publish remoteClient (fromJust . replyTo $ msg) [withPayload "WORLD"]
                unsubscribe remoteClient (sid msg)
          promptClient <- newClient [(natsHost, natsPort)] [withConnectName "6eff2527-1ad5-4b0c-b4e5-4a52a7d17639", withLoggerConfig logger]
          replyBox <- newEmptyTMVarIO
          publish promptClient topic [withReplyCallback (atomically . putTMVar replyBox), withPayload "HELLO"]
          reply <- atomically $ readTMVar replyBox
          case reply of
            Nothing  -> expectationFailure "Expected a reply but got Nothing"
            Just msg -> payload msg `shouldBe` Just "WORLD"
          close remoteClient
          close promptClient
        it "cycles through servers"  $ \(Endpoints natsHost natsPort) -> do
          c <- newClient [("0.0.0.0", 4999), (natsHost, natsPort)] [withConnectName "b896f0fb-ea45-4456-86d9-b7d6269eb75f", withLoggerConfig logger, withConnectionAttempts 2]
          wg <- newWaitGroup 1
          ping c $ done wg
          wait wg
          close c
        it "exits when no valid servers" $ \(Endpoints _ _) -> do
          wg <- newWaitGroup 1
          exitResult <- newEmptyTMVarIO
          newClient [("0.0.0.0", 4999)] [
            withConnectName "9b694d4e-7b78-459c-9126-57e582564a0b",
            withExitAction (\r -> atomically (putTMVar exitResult r) >> done wg),
            withLoggerConfig logger
            ]
          wait wg
          result <- atomically $ readTMVar exitResult
          exitStatus result `shouldBe` ExitStatusRetryExhausted
          exitCode result `shouldBe` 1
          case exitReason result of
            ExitRetriesExhausted _ -> pure ()
            other                  -> expectationFailure $ "Unexpected exit reason: " ++ show other
  describe "client auth" $ do
    logger <- runIO testLoggerConfig
    cases <- runIO loadAuthCases
    around_ (\action -> action >> breakLogFile "nats.log") $ do
      mapM_ (authSpec logger) cases
