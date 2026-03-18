{-# LANGUAGE OverloadedStrings #-}

module ClientFatalErrorSpec (spec) where

import           Client
import           Control.Concurrent.STM
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec =
  clientSystemTest "446a4bb1-fd43-4adc-9a22-3cbfd53d6016" "exits immediately on fatal error" $ \loggerOptions _ -> do
    wg <- newWaitGroup 1
    exitResult <- newEmptyTMVarIO
    _ <- newClient [("0.0.0.0", 4999)] $
      [ withConnectName "fatal-reset-test"
      , withExitAction (\r -> atomically (putTMVar exitResult r) >> done wg)
      ]
      ++ loggerOptions
    wait wg
    result <- atomically $ readTMVar exitResult
    case result of
      ExitRetriesExhausted _ -> pure ()
      other                  -> expectationFailure $ "Unexpected exit reason: " ++ show other
