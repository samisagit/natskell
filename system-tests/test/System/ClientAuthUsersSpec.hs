{-# LANGUAGE OverloadedStrings #-}

module ClientAuthUsersSpec (spec) where

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
      let serverOptions =
            [ WithLogVerbosity NatsLogDebug
            , WithUsers [NatsAuthUserPass "test-user" "test-pass"]
            ]
      around (withNatsContainerConfigNamed "cb6e2a23-4ff3-42d7-8b89-6c8cbad66223" serverOptions) $ do
        it "authenticates with users list" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          pinged <- newEmptyTMVarIO
          let clientOptions =
                [ withConnectName "auth-users"
                , withExitAction (atomically . putTMVar exitResult)
                , withUserPass ("test-user", "test-pass")
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
