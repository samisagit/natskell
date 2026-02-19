{-# LANGUAGE OverloadedStrings #-}

module ClientNoServersSpec (spec) where

import           Client
import           Control.Concurrent.STM
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec =
  clientSystemTest "exits when no valid servers" $ \logger _ -> do
    wg <- newWaitGroup 1
    exitResult <- newEmptyTMVarIO
    newClient [("0.0.0.0", 4999)]
      [ withConnectName "9b694d4e-7b78-459c-9126-57e582564a0b"
      , withExitAction (\r -> atomically (putTMVar exitResult r) >> done wg)
      , withLoggerConfig logger
      ]
    wait wg
    result <- atomically $ readTMVar exitResult
    case result of
      ExitRetriesExhausted _ -> pure ()
      other                  -> expectationFailure $ "Unexpected exit reason: " ++ show other
