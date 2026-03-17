{-# LANGUAGE OverloadedStrings #-}

module ClientAuthNKeySpec (spec) where

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
      nkeyPub <- runIO (readFixtureText "nkey/user.pub")
      nkeySeed <- runIO (readFixtureBytesTrim "nkey/user.seed")
      let serverOptions =
            [ WithLogVerbosity NatsLogDebug
            , WithNKey nkeyPub
            ]
      around (withNatsContainerConfigNamed "5b9c5c3e-1e91-4d3c-8e8a-2c6fe2f7b4a0" serverOptions) $ do
        it "authenticates with nkey" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          pinged <- newEmptyTMVarIO
          let clientOptions =
                [ withConnectName "auth-nkey"
                , withExitAction (atomically . putTMVar exitResult)
                , withNKey nkeySeed
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
