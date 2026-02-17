{-# LANGUAGE OverloadedStrings #-}

module ClientCycleSpec (spec) where

import           Client
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec =
  clientSystemTest "cycles through servers" $ \logger (Endpoints natsHost natsPort) -> do
    c <- newClient [("0.0.0.0", 4999), (natsHost, natsPort)]
      [ withConnectName "b896f0fb-ea45-4456-86d9-b7d6269eb75f"
      , withLoggerConfig logger
      , withConnectionAttempts 2
      ]
    wg <- newWaitGroup 1
    ping c $ done wg
    wait wg
    close c
