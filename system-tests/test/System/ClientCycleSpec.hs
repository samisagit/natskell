{-# LANGUAGE OverloadedStrings #-}

module ClientCycleSpec (spec) where

import           API         (Client (..))
import           Client
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec =
  clientSystemTest "33e0d730-067d-4b42-b741-82ac7c1d308f" "cycles through servers" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    c <- newClient [("0.0.0.0", 4999), (natsHost, natsPort)] $
      [ withConnectName "b896f0fb-ea45-4456-86d9-b7d6269eb75f"
      , withConnectionAttempts 2
      ]
      ++ loggerOptions
    wg <- newWaitGroup 1
    ping c $ done wg
    wait wg
    close c
