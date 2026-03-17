{-# LANGUAGE OverloadedStrings #-}

module ClientPingSpec (spec) where

import           API         (Client (..))
import           Client
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec =
  clientSystemTest "e0b6e5db-5154-47a0-b1b7-e83c0a4951c3" "PING results in PONG" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    c <- newClient [(natsHost, natsPort)] $
      withConnectName "1f27aec6-e832-41ad-88ad-15555985b754"
        : loggerOptions
    wg <- newWaitGroup 1
    ping c $ done wg
    wait wg
    close c
