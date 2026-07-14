{-# LANGUAGE OverloadedStrings #-}

module ClientAuthTlsCertSpec (spec) where

import           API
import           Client
import           Control.Concurrent     (forkIO)
import           Control.Concurrent.STM
import           NatsServerConfig
import           Test.Hspec
import           TestSupport

spec :: Spec
spec =
  systemTest . describe "client auth" $ do
      let loggerOptions = testLoggerOptions
      clientCert <- runIO (readFixtureBytesRaw "tls/client.crt")
      clientKey <- runIO (readFixtureBytesRaw "tls/client.key")
      tlsRoot <- runIO (readFixtureBytesRaw "tls/ca.crt")
      tlsHostDir <- runIO (fixturePath "tls")
      let tlsContainerDir = "/etc/nats/certs"
      let tlsConfig =
            NatsTlsConfig
              { natsTlsCertFile = tlsContainerDir ++ "/server.crt"
              , natsTlsKeyFile = tlsContainerDir ++ "/server.key"
              , natsTlsCaFile = Just (tlsContainerDir ++ "/ca.crt")
              , natsTlsVerify = True
              , natsTlsVerifyAndMap = True
              , natsTlsTimeout = Just 1
              }
      let serverOptions =
            [ WithLogVerbosity NatsLogDebug
            , WithUsers [NatsAuthUserName "CN=natskell-test-client"]
            , WithTlsConfig tlsConfig
            ]
      let mounts = [(tlsHostDir, tlsContainerDir)]
      around (withNatsContainerConfigWithMountsNamed "a4338dc4-7755-4604-ab84-329f30f40db6" serverOptions mounts) $ do
        it "authenticates with tls cert" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          pinged <- newEmptyTMVarIO
          let clientOptions =
                [ withConnectName "auth-tls"
                , withExitAction (atomically . putTMVar exitResult)
                , withTLSCert (clientCert, clientKey)
                , withTLSRootCA tlsRoot
                , withConnectionAttempts 1
                ]
                ++ loggerOptions
          client <- newTestClient [(natsHost, natsPort)] clientOptions
          forkIO $ do
            outcome <- atomically $ (Left <$> readTMVar pinged) `orElse` (Right <$> readTMVar exitResult)
            case outcome of
              Left _  -> close client []
              Right _ -> pure ()
          _ <- ping client []
          atomically (putTMVar pinged ())
          result <- atomically $ readTMVar exitResult
          result `shouldBe` ExitClosedByUser
        it "rejects tls client without required certificate" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          let clientOptions =
                [ withConnectName "auth-tls-missing-client-cert"
                , withExitAction (atomically . putTMVar exitResult)
                , withTLSRootCA tlsRoot
                , withConnectionAttempts 1
                ]
                ++ loggerOptions
          connectResult <- newClient [(natsHost, natsPort)] clientOptions
          case connectResult of
            Right client -> do
              close client []
              expectationFailure "TLS client connected without a required certificate"
            Left _ ->
              pure ()
