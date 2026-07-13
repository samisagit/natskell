{-# LANGUAGE OverloadedStrings #-}

module ClientAuthTlsTokenSpec (spec) where

import           API                    (Client (..))
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
      tlsRoot <- runIO (readFixtureBytesRaw "tls/ca.crt")
      tlsHostDir <- runIO (fixturePath "tls")
      let tlsContainerDir = "/etc/nats/certs"
      let tlsConfig =
            NatsTlsConfig
              { natsTlsCertFile = tlsContainerDir ++ "/server.crt"
              , natsTlsKeyFile = tlsContainerDir ++ "/server.key"
              , natsTlsCaFile = Nothing
              , natsTlsVerify = False
              , natsTlsVerifyAndMap = False
              , natsTlsTimeout = Just 1
              }
      let serverOptions =
            [ WithLogVerbosity NatsLogDebug
            , WithToken "test-token"
            , WithTlsConfig tlsConfig
            ]
      let mounts = [(tlsHostDir, tlsContainerDir)]
      around (withNatsContainerConfigWithMountsNamed "4f3d8d3a-93a2-45d1-a6fd-77efdb4d0aec" serverOptions mounts) $ do
        it "authenticates with tls and token" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          pinged <- newEmptyTMVarIO
          let clientOptions =
                [ withConnectName "auth-tls-token"
                , withExitAction (atomically . putTMVar exitResult)
                , withAuthToken "test-token"
                , withTLSRootCA tlsRoot
                , withConnectionAttempts 1
                ]
                ++ loggerOptions
          client <- newTestClient [(natsHost, natsPort)] clientOptions
          forkIO $ do
            outcome <- atomically $ (Left <$> readTMVar pinged) `orElse` (Right <$> readTMVar exitResult)
            case outcome of
              Left _  -> close client
              Right _ -> pure ()
          ping client (atomically (putTMVar pinged ()))
          result <- atomically $ readTMVar exitResult
          result `shouldBe` ExitClosedByUser
        it "rejects an untrusted tls server" $ \(Endpoints natsHost natsPort) -> do
          connectResult <- newClient [(natsHost, natsPort)]
            [ withAuthToken "test-token"
            , withConnectionAttempts 1
            ]
          case connectResult of
            Left (ConnectAttemptsExhausted [ConnectAttemptError _ (ConnectTLSFailure _)]) ->
              pure ()
            Left err ->
              expectationFailure ("unexpected connection error: " ++ show err)
            Right client -> do
              close client
              expectationFailure "connected to an untrusted tls server"
