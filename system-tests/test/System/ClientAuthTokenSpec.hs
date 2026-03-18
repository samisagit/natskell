{-# LANGUAGE OverloadedStrings #-}

module ClientAuthTokenSpec (spec) where

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
            , WithToken "test-token"
            ]
      around (withNatsContainerConfigNamed "7b4c2c96-6c9b-4d1d-8f84-9f6c2d18e3a1" serverOptions) $ do
        it "authenticates with token" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          pinged <- newEmptyTMVarIO
          let clientOptions =
                [ withConnectName "auth-token"
                , withExitAction (atomically . putTMVar exitResult)
                , withAuthToken "test-token"
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
