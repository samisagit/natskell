{-# LANGUAGE OverloadedStrings #-}

module ClientCloseSpec (spec) where

import           API                    (Client (..))
import           Client
import           Control.Concurrent.STM
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec =
  clientSystemTest "c5f9a552-2624-4af2-9d1a-4cc8a7e45b90" "user can close connection" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    wg <- newWaitGroup 1
    exitResult <- newEmptyTMVarIO
    client <- newClient [(natsHost, natsPort)] $
      [ withConnectName "b9ed73e3-9674-41a2-9979-bb63b78c6579"
      , withExitAction (\r -> atomically (putTMVar exitResult r) >> done wg)
      ]
      ++ loggerOptions
    close client
    wait wg
    result <- atomically $ readTMVar exitResult
    result `shouldBe` ExitClosedByUser
