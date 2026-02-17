{-# LANGUAGE OverloadedStrings #-}

module ClientCloseSpec (spec) where

import           Client
import           Control.Concurrent.STM
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec =
  clientSystemTest "user can close connection" $ \logger (Endpoints natsHost natsPort) -> do
    wg <- newWaitGroup 1
    exitResult <- newEmptyTMVarIO
    client <- newClient [(natsHost, natsPort)]
      [ withConnectName "b9ed73e3-9674-41a2-9979-bb63b78c6579"
      , withExitAction (\r -> atomically (putTMVar exitResult r) >> done wg)
      , withLoggerConfig logger
      ]
    close client
    wait wg
    result <- atomically $ readTMVar exitResult
    result `shouldBe` ExitClosedByUser
