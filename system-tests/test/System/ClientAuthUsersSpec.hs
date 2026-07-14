{-# LANGUAGE OverloadedStrings #-}

module ClientAuthUsersSpec (spec) where

import           API
import           Client
import           Control.Concurrent     (forkIO)
import           Control.Concurrent.STM
import           Control.Exception      (finally)
import           Data.List              (isInfixOf)
import           NatsServerConfig
import           System.Timeout         (timeout)
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
      let permissionServerOptions =
            [ WithLogVerbosity NatsLogDebug
            , WithUsers
                [ NatsAuthUserPassWithPermissions
                    "publish-limited"
                    "test-pass"
                    NatsUserPermissions
                      { natsUserPublishPermissions =
                          Just (NatsSubjectPermission ["PERMISSIONS.ALLOWED"] [])
                      , natsUserSubscribePermissions =
                          Just (NatsSubjectPermission [">"] [])
                      }
                , NatsAuthUserPassWithPermissions
                    "subscribe-limited"
                    "test-pass"
                    NatsUserPermissions
                      { natsUserPublishPermissions =
                          Just (NatsSubjectPermission [">"] [])
                      , natsUserSubscribePermissions =
                          Just (NatsSubjectPermission ["PERMISSIONS.ALLOWED"] [])
                      }
                ]
            ]
      around (withNatsContainerConfigNamed "24a75245-348f-48ba-9f8e-3b0fe216ec9f" permissionServerOptions) $ do
        it "reports publish permission violations" $ \(Endpoints natsHost natsPort) -> do
          logs <- newTVarIO []
          let recordLog entry =
                atomically $ modifyTVar' logs (entry:)
              clientOptions =
                [ withConnectName "auth-users-publish-denied"
                , withUserPass ("publish-limited", "test-pass")
                , withMinimumLogLevel Debug
                , withLogAction recordLog
                ]
          client <- newTestClient [(natsHost, natsPort)] clientOptions
          (do
              _ <- publish client "PERMISSIONS.DENIED" "blocked" []
              flush client []
              waitForLogContaining logs "Permissions Violation")
            `finally` close client []

        it "reports subscribe permission violations" $ \(Endpoints natsHost natsPort) -> do
          logs <- newTVarIO []
          let recordLog entry =
                atomically $ modifyTVar' logs (entry:)
              clientOptions =
                [ withConnectName "auth-users-subscribe-denied"
                , withUserPass ("subscribe-limited", "test-pass")
                , withMinimumLogLevel Debug
                , withLogAction recordLog
                ]
          client <- newTestClient [(natsHost, natsPort)] clientOptions
          (do
              _ <- subscribe client "PERMISSIONS.DENIED" [] (const (pure ()))
              flush client []
              waitForLogContaining logs "Permissions Violation")
            `finally` close client []

waitForLogContaining :: TVar [LogEntry] -> String -> IO ()
waitForLogContaining logs needle = do
  found <- timeout (5 * 1000000) (atomically waitForNeedle)
  found `shouldBe` Just ()
  where
    waitForNeedle = do
      entries <- readTVar logs
      case any (isInfixOf needle . leMessage) entries of
        True  -> pure ()
        False -> retry
