{-# LANGUAGE OverloadedStrings #-}

module ClientAuthTlsUserPassSpec (spec) where

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
            , WithUserPass "test-user" "test-pass"
            , WithTlsConfig tlsConfig
            ]
      let mounts = [(tlsHostDir, tlsContainerDir)]
      around (withNatsContainerConfigWithMountsNamed "84ec2557-17dc-4e8b-994b-553e3d0f7880" serverOptions mounts) $ do
        it "authenticates with tls and user/pass" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          pinged <- newEmptyTMVarIO
          let clientOptions =
                [ withConnectName "auth-tls-user-pass"
                , withExitAction (atomically . putTMVar exitResult)
                , withUserPass ("test-user", "test-pass")
                , withConnectionAttempts 1
                ]
                ++ loggerOptions
          client <- newClient [(natsHost, natsPort)] clientOptions
          forkIO $ do
            outcome <- atomically $ (Left <$> readTMVar pinged) `orElse` (Right <$> readTMVar exitResult)
            case outcome of
              Left _  -> close client
              Right _ -> pure ()
          ping client (atomically (putTMVar pinged ()))
          result <- atomically $ readTMVar exitResult
          result `shouldBe` ExitClosedByUser
