{-# LANGUAGE OverloadedStrings #-}

module ClientAuthNKeySpec (spec) where

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
      let serverOptions =
            [ WithLogVerbosity NatsLogDebug
            , WithNKey nkeyPub
            ]
      around (withNatsContainerConfig serverOptions) $ do
        it "authenticates with nkey" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          pinged <- newEmptyTMVarIO
          let clientOpts =
                [ withConnectName "auth-nkey"
                , withExitAction (atomically . putTMVar exitResult)
                , withLoggerConfig logger
                , withNKey nkeySeed
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
