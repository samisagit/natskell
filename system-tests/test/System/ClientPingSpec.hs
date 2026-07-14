{-# LANGUAGE OverloadedStrings #-}

module ClientPingSpec (spec) where

import           API
import           Client
import           Control.Exception (finally)
import           System.Timeout
import           Test.Hspec
import           TestSupport

spec :: Spec
spec = do
  clientSystemTest "e0b6e5db-5154-47a0-b1b7-e83c0a4951c3" "PING results in PONG" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    c <- newTestClient [(natsHost, natsPort)] $
      withConnectName "1f27aec6-e832-41ad-88ad-15555985b754"
        : loggerOptions
    ping c [] `shouldReturn` Right ()
    close c []
  clientSystemTest "248307ca-8d2e-4cf4-b7e2-0d29d2b5fd4d" "flush and ping report a closed client" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    c <- newTestClient [(natsHost, natsPort)] $
      withConnectName "ping-after-close"
        : loggerOptions
    close c []
    flushed <- timeout (1 * 1000000) (flush c [])
    flushed `shouldSatisfy` maybe False isClosed
    pinged <- timeout (1 * 1000000) (ping c [])
    pinged `shouldSatisfy` maybe False isClosed
  clientSystemTest "de018bd0-1fd4-4247-afb8-7848993502c3" "ping and flush work after reconnect" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    c <- newTestClient [(natsHost, natsPort)] $
      withConnectName "ping-after-reconnect"
        : loggerOptions
    (do
        reset c []
        flushed <- timeout (5 * 1000000) (flush c [])
        flushed `shouldBe` Just (Right ())
        pinged <- timeout (5 * 1000000) (ping c [])
        pinged `shouldBe` Just (Right ()))
      `finally` close c []

isClosed :: Either NatsError () -> Bool
isClosed (Left (NatsConnectionClosed _)) = True
isClosed _                               = False
