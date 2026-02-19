{-# LANGUAGE OverloadedStrings #-}

module ClientAuthTlsNKeySpec (spec) where

import           Client
import           Control.Concurrent     (forkIO)
import           Control.Concurrent.STM
import           NatsServerConfig
import           Test.Hspec
import           TestSupport

spec :: Spec
spec =
  systemTest . describe "client auth" $ do
      logger <- runIO testLoggerConfig
      nkeyPub <- runIO (readFixtureText "nkey/user.pub")
      nkeySeed <- runIO (readFixtureBytesTrim "nkey/user.seed")
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
            , WithNKey nkeyPub
            , WithTlsConfig tlsConfig
            ]
      let mounts = [(tlsHostDir, tlsContainerDir)]
      around (withNatsContainerConfigWithMounts serverOptions mounts) $ do
        it "authenticates with tls and nkey" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          pinged <- newEmptyTMVarIO
          let clientOpts =
                [ withConnectName "auth-tls-nkey"
                , withExitAction (atomically . putTMVar exitResult)
                , withLoggerConfig logger
                , withNKey nkeySeed
                , withConnectionAttempts 1
                ]
          client <- newClient [(natsHost, natsPort)] clientOpts
          forkIO $ do
            outcome <- atomically $ (Left <$> readTMVar pinged) `orElse` (Right <$> readTMVar exitResult)
            case outcome of
              Left _  -> close client
              Right _ -> pure ()
          ping client (atomically (putTMVar pinged ()))
          result <- atomically $ readTMVar exitResult
          result `shouldBe` ExitClosedByUser
