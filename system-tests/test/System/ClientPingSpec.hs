{-# LANGUAGE OverloadedStrings #-}

module ClientPingSpec (spec) where

import           API                (Client (..))
import           Client
import           Control.Concurrent
import           Control.Exception  (finally)
import           System.Timeout
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec = do
  clientSystemTest "e0b6e5db-5154-47a0-b1b7-e83c0a4951c3" "PING results in PONG" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    c <- newClient [(natsHost, natsPort)] $
      withConnectName "1f27aec6-e832-41ad-88ad-15555985b754"
        : loggerOptions
    wg <- newWaitGroup 1
    ping c $ done wg
    wait wg
    close c
  clientSystemTest "248307ca-8d2e-4cf4-b7e2-0d29d2b5fd4d" "flush returns and ping is ignored after close" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    c <- newClient [(natsHost, natsPort)] $
      withConnectName "ping-after-close"
        : loggerOptions
    close c
    flushed <- timeout (1 * 1000000) (flush c)
    flushed `shouldBe` Just ()
    pinged <- newEmptyMVar
    ping c (putMVar pinged ())
    callback <- timeout 300000 (takeMVar pinged)
    callback `shouldBe` Nothing
  clientSystemTest "de018bd0-1fd4-4247-afb8-7848993502c3" "ping and flush work after reconnect" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    c <- newClient [(natsHost, natsPort)] $
      withConnectName "ping-after-reconnect"
        : loggerOptions
    (do
        reset c
        flushed <- timeout (5 * 1000000) (flush c)
        flushed `shouldBe` Just ()
        pinged <- newEmptyMVar
        ping c (putMVar pinged ())
        callback <- timeout (5 * 1000000) (takeMVar pinged)
        callback `shouldBe` Just ())
      `finally` close c
