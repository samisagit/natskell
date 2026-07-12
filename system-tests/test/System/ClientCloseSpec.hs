{-# LANGUAGE OverloadedStrings #-}

module ClientCloseSpec (spec) where

import           API                    (Client (..), withPayload)
import           Client
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception      (finally)
import           System.Timeout
import           Test.Hspec
import           TestSupport
import           WaitGroup

spec :: Spec
spec = do
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
  clientSystemTest "12d990bc-331f-409e-9cdb-6e8b7f419050" "close waits for in-flight subscription callbacks" $ \loggerOptions (Endpoints natsHost natsPort) -> do
    let topic = "CLOSE.DRAIN"
    callbackStarted <- newEmptyMVar
    releaseCallback <- newEmptyMVar
    callbackFinished <- newEmptyMVar
    closeReturned <- newEmptyMVar
    exitResult <- newEmptyTMVarIO
    subscriber <- newClient [(natsHost, natsPort)] $
      [ withConnectName "close-drain-subscriber"
      , withExitAction (atomically . putTMVar exitResult)
      ]
      ++ loggerOptions
    publisher <- newClient [(natsHost, natsPort)] $
      withConnectName "close-drain-publisher"
        : loggerOptions
    let cleanup = do
          _ <- tryPutMVar releaseCallback ()
          close subscriber
          close publisher
    (do
        _ <- subscribe subscriber topic [] $ \_ -> do
          putMVar callbackStarted ()
          takeMVar releaseCallback
          putMVar callbackFinished ()
        flush subscriber
        publish publisher topic [withPayload "wait-for-callback"]
        flush publisher
        started <- timeout (5 * 1000000) (takeMVar callbackStarted)
        started `shouldBe` Just ()
        _ <- forkIO $ close subscriber >> putMVar closeReturned ()
        earlyClose <- timeout 300000 (takeMVar closeReturned)
        case earlyClose of
          Nothing ->
            pure ()
          Just () ->
            expectationFailure "close returned before callback completed"
        putMVar releaseCallback ()
        closed <- timeout (5 * 1000000) (takeMVar closeReturned)
        closed `shouldBe` Just ()
        finished <- timeout (5 * 1000000) (takeMVar callbackFinished)
        finished `shouldBe` Just ()
        result <- atomically $ readTMVar exitResult
        result `shouldBe` ExitClosedByUser)
      `finally` cleanup
