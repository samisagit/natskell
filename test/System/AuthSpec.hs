{-# LANGUAGE OverloadedStrings #-}

module AuthSpec (spec) where

import           Client
import           Control.Concurrent.STM
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import           System.Directory       (getCurrentDirectory)
import           Test.Hspec
import qualified TestContainers.Hspec   as TC
import           WaitGroup

data AuthEndpoints = AuthEndpoints
                       { authNatsHost :: String
                       , authNatsPort :: Int
                       }

-- | Test JWT token for TestUser
testJWT :: BS.ByteString
testJWT = "eyJ0eXAiOiJKV1QiLCJhbGciOiJlZDI1NTE5LW5rZXkifQ.eyJqdGkiOiJWVlROUUFHTTVBU05WR1gzUENVTFhZS0JZSFkzRkhOVU5HTUdMVzZKU0xDUkNKRERNSVNRIiwiaWF0IjoxNzY5MDk3MzA3LCJpc3MiOiJBQ1hUVVVJNVE2M0dDTFZDSEFFWU0zRk02RVRKWkZIUkE3WE1KNVRDV0dORFhJS1FVVVVHUUFNTSIsIm5hbWUiOiJUZXN0VXNlciIsInN1YiI6IlVET1NOQkJBWkMyVFg2QklJTkRVTEhPMzRLRjQ0QkVGWEJZNkxJR0xYSloySEtWM0ZSREJLMjdOIiwibmF0cyI6eyJwdWIiOnt9LCJzdWIiOnt9LCJzdWJzIjotMSwiZGF0YSI6LTEsInBheWxvYWQiOi0xLCJ0eXBlIjoidXNlciIsInZlcnNpb24iOjJ9fQ.B-AsZ9n2tKoZPXBqt35M5y-cCGFctsKpMTHyzH7LHAH8MXMvUrt17W2GmftcHQ-FWNHZw_1PyW3NNxobGlKxDw"

-- | Test NKey seed for TestUser
testNKey :: BS.ByteString
testNKey = "SUAJUTQP57YCTCOU34H52A7XQ7ECNYZMUSWZTUB3U2645J6KBYJ2RBZOQQ"

testLoggerConfig :: IO LoggerConfig
testLoggerConfig = do
  lock <- newTMVarIO ()
  pure $ LoggerConfig Debug (\_ s -> putStrLn s) lock

-- | Container configured with JWT auth using memory resolver
authContainer :: TC.TestContainer AuthEndpoints
authContainer = do
    cwd <- liftIO getCurrentDirectory
    let fixturesPath = T.pack $ cwd ++ "/test/System/fixtures"
    natsContainer <- TC.run (TC.containerRequest
      (TC.fromTag "nats:latest")
      TC.& TC.setExpose [ 4222 ]
      TC.& TC.setWaitingFor (TC.waitUntilMappedPortReachable 4222)
      -- Mount the server config file with absolute path
      TC.& TC.setVolumeMounts [(fixturesPath, "/config")]
      TC.& TC.setCmd ["-c", "/config/server.conf", "-DV"]
      )
    pure $ AuthEndpoints
      { authNatsHost = "0.0.0.0"
      , authNatsPort = TC.containerPort natsContainer 4222
      }

spec :: Spec
spec = do
  describe "JWT + NKey authentication" $ do
    logger <- runIO testLoggerConfig
    around (TC.withContainers authContainer) $ do
      it "connects successfully with valid JWT and NKey" $ \(AuthEndpoints host port) -> do
        wg <- newWaitGroup 1
        client <- newClient [(host, port)]
          [ withJWTCreds testJWT testNKey
          , withLoggerConfig logger
          , withConnectName "jwt-nkey-test-client"
          ]
        -- Verify connection succeeded by doing a ping
        ping client (done wg)
        wait wg
        close client
