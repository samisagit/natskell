{-# LANGUAGE OverloadedStrings #-}

module ClientFatalErrorSpec (spec) where

import           Client
import           Control.Concurrent.STM
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec =
  clientSystemTest "exits immediately on fatal error" $ \logger _ -> do
    wg <- newWaitGroup 1
    exitResult <- newEmptyTMVarIO
    _ <- newClient [("0.0.0.0", 4999)]
      [ withConnectName "fatal-reset-test"
      , withExitAction (\r -> atomically (putTMVar exitResult r) >> done wg)
      , withLoggerConfig logger
      ]
    wait wg
    result <- atomically $ readTMVar exitResult
    case result of
      ExitRetriesExhausted _ -> pure ()
      other                  -> expectationFailure $ "Unexpected exit reason: " ++ show other
