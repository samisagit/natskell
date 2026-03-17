{-# LANGUAGE OverloadedStrings #-}

module ClientAuthUserPassSpec (spec) where

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
            , WithUserPass "test-user" "test-pass"
            ]
      around (withNatsContainerConfigNamed "d5f07159-cabf-4f62-b4e5-d8be7ce39444" serverOptions) $ do
        it "authenticates with user/pass" $ \(Endpoints natsHost natsPort) -> do
          exitResult <- newEmptyTMVarIO
          pinged <- newEmptyTMVarIO
          let clientOptions =
                [ withConnectName "auth-user-pass"
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
