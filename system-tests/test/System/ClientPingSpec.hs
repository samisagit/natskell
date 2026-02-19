{-# LANGUAGE OverloadedStrings #-}

module ClientPingSpec (spec) where

import           Client
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec =
  clientSystemTest "PING results in PONG" $ \logger (Endpoints natsHost natsPort) -> do
    c <- newClient [(natsHost, natsPort)]
      [ withConnectName "1f27aec6-e832-41ad-88ad-15555985b754"
      , withLoggerConfig logger
      ]
    wg <- newWaitGroup 1
    ping c $ done wg
    wait wg
    close c
