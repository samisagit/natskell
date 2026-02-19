{-# LANGUAGE OverloadedStrings #-}

module ClientAuthUsersSpec (spec) where

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
      let serverOptions =
            [ WithLogVerbosity NatsLogDebug
            , WithUsers [NatsAuthUserPass "test-user" "test-pass"]
            ]
      around (withNatsContainerConfig serverOptions) $ do
        it "authenticates with users list" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          pinged <- newEmptyTMVarIO
          let clientOpts =
                [ withConnectName "auth-users"
                , withExitAction (atomically . putTMVar exitResult)
                , withLoggerConfig logger
                , withUserPass ("test-user", "test-pass")
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
