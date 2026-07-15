{-# LANGUAGE OverloadedStrings #-}

module SubscriptionStoreSpec (spec) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Subscription.Store
import           Subscription.Types
import           Test.Hspec
import           Types.Msg

spec :: Spec
spec = do
  describe "terminal cleanup" $ do
    it "clears active and expiry state while preserving queued callbacks" $ do
      delivered <- newEmptyMVar
      stopping <- newTVarIO False
      store <- newSubscriptionStore defaultPendingLimits (pure ())
      register
        store
        "1"
        (SubscriptionMeta "A" Nothing StandardSubscription)
        (SubscribeConfig Nothing Nothing)
        (const (putMVar delivered ()))
        (pure ())
      register
        store
        "2"
        (SubscriptionMeta "B" Nothing OneShotSubscription)
        (SubscribeConfig (Just 60) Nothing)
        (const (pure ()))
        (pure ())
      dispatchMessage store (message "1" "one") `shouldReturn` DispatchQueued

      closeStore store

      active store `shouldReturn` []
      hasTrackedExpiries store `shouldReturn` False
      _ <- startWorkers
        1
        store
        (readTVar stopping >>= check)
        (const (pure ()))
      takeMVar delivered
      atomically (awaitCallbackDrain store)
      atomically (writeTVar stopping True)

  describe "global pending delivery limits" $ do
    it "shares the message limit across subscriptions and coalesces notification" $ do
      slowEvents <- newIORef (0 :: Int)
      firstOverflow <- newIORef (0 :: Int)
      secondOverflow <- newIORef (0 :: Int)
      store <-
        newSubscriptionStore
          (PendingLimits 1 1024)
          (modifyIORef' slowEvents (+ 1))
      registerSubscription store "1" "A" (modifyIORef' firstOverflow (+ 1))
      registerSubscription store "2" "B" (modifyIORef' secondOverflow (+ 1))

      dispatchMessage store (message "1" "one")
        `shouldReturn` DispatchQueued
      dispatchMessage store (message "2" "two")
        `shouldReturn` DispatchDropped True
      dispatchMessage store (message "2" "three")
        `shouldReturn` DispatchDropped False

      readIORef slowEvents `shouldReturn` 0
      readIORef firstOverflow `shouldReturn` 0
      readIORef secondOverflow `shouldReturn` 2

    it "rejects a delivery that exceeds the global byte limit" $ do
      store <- newSubscriptionStore (PendingLimits 10 4) (pure ())
      registerSubscription store "1" "A" (pure ())

      dispatchMessage store (message "1" "12345")
        `shouldReturn` DispatchDropped True

    it "re-enables notification after the backlog recovers" $ do
      slowEvents <- newIORef (0 :: Int)
      callbackStarted <- newEmptyMVar
      releaseCallback <- newEmptyMVar
      stopping <- newTVarIO False
      store <-
        newSubscriptionStore
          (PendingLimits 1 1024)
          (modifyIORef' slowEvents (+ 1))
      register
        store
        "1"
        (SubscriptionMeta "A" Nothing StandardSubscription)
        (SubscribeConfig Nothing Nothing)
        (\_ -> putMVar callbackStarted () >> takeMVar releaseCallback)
        (pure ())
      startWorkers
        1
        store
        (readTVar stopping >>= check)
        (const (pure ()))

      dispatchMessage store (message "1" "one")
        `shouldReturn` DispatchQueued
      takeMVar callbackStarted
      dispatchMessage store (message "1" "two")
        `shouldReturn` DispatchDropped True
      putMVar releaseCallback ()
      atomically (awaitCallbackDrain store)
      readIORef slowEvents `shouldReturn` 1

      dispatchMessage store (message "1" "three")
        `shouldReturn` DispatchQueued
      takeMVar callbackStarted
      dispatchMessage store (message "1" "four")
        `shouldReturn` DispatchDropped True
      putMVar releaseCallback ()
      atomically (awaitCallbackDrain store)
      readIORef slowEvents `shouldReturn` 2
      atomically (writeTVar stopping True)

    it "releases capacity when a callback throws" $ do
      callbackFailed <- newEmptyMVar
      stopping <- newTVarIO False
      store <- newSubscriptionStore (PendingLimits 1 1024) (pure ())
      register
        store
        "1"
        (SubscriptionMeta "A" Nothing StandardSubscription)
        (SubscribeConfig Nothing Nothing)
        (const (throwIO (userError "callback failed")))
        (pure ())
      startWorkers
        1
        store
        (readTVar stopping >>= check)
        (putMVar callbackFailed)

      dispatchMessage store (message "1" "one")
        `shouldReturn` DispatchQueued
      void (takeMVar callbackFailed)
      atomically (awaitCallbackDrain store)
      dispatchMessage store (message "1" "two")
        `shouldReturn` DispatchQueued
      void (takeMVar callbackFailed)
      atomically (awaitCallbackDrain store)
      atomically (writeTVar stopping True)

    it "removes a one-shot subscription whose delivery is dropped" $ do
      store <- newSubscriptionStore (PendingLimits 1 1024) (pure ())
      registerSubscription store "1" "A" (pure ())
      register
        store
        "2"
        (SubscriptionMeta "B" Nothing OneShotSubscription)
        (SubscribeConfig Nothing Nothing)
        (const (pure ()))
        (pure ())

      dispatchMessage store (message "1" "one")
        `shouldReturn` DispatchQueued
      dispatchMessage store (message "2" "two")
        `shouldReturn` DispatchDropped True
      dispatchMessage store (message "2" "two")
        `shouldReturn` DispatchMissing

    it "accepts a dropped one-shot before reporting overflow" $ do
      accepted <- newTVarIO (0 :: Int)
      store <- newSubscriptionStore (PendingLimits 1 1024) (pure ())
      registerSubscription store "1" "A" (pure ())
      registerWithAcceptance
        store
        "2"
        (SubscriptionMeta "B" Nothing OneShotSubscription)
        (SubscribeConfig Nothing Nothing)
        (modifyTVar' accepted (+ 1))
        (const (pure ()))
        (pure ())

      dispatchMessage store (message "1" "one")
        `shouldReturn` DispatchQueued
      dispatchMessage store (message "2" "two")
        `shouldReturn` DispatchDropped True
      readTVarIO accepted `shouldReturn` 1
      dispatchMessage store (message "2" "duplicate")
        `shouldReturn` DispatchMissing
      readTVarIO accepted `shouldReturn` 1

    it "commits request acceptance and overflow atomically" $ do
      accepted <- newTVarIO False
      rejected <- newTVarIO False
      throwOnRejection <- newTVarIO True
      store <- newSubscriptionStore (PendingLimits 1 1024) (pure ())
      registerSubscription store "1" "A" (pure ())
      registerWithDispatchHooks
        store
        "2"
        (SubscriptionMeta "B" Nothing RequestReplySubscription)
        (SubscribeConfig Nothing Nothing)
        (writeTVar accepted True)
        (do
          shouldThrow <- readTVar throwOnRejection
          when shouldThrow (throwSTM (userError "rejection failed"))
          writeTVar rejected True)
        (const (pure ()))
        (pure ())

      dispatchMessage store (message "1" "one")
        `shouldReturn` DispatchQueued
      dispatchMessage store (message "2" "two")
        `shouldThrow` anyIOException
      atomically ((,) <$> readTVar accepted <*> readTVar rejected)
        `shouldReturn` (False, False)
      map fst <$> active store `shouldReturn` ["1", "2"]

      atomically (writeTVar throwOnRejection False)
      dispatchMessage store (message "2" "two")
        `shouldReturn` DispatchDropped True
      atomically ((,) <$> readTVar accepted <*> readTVar rejected)
        `shouldReturn` (True, True)

registerSubscription
  :: SubscriptionStore
  -> SID
  -> Subject
  -> IO ()
  -> IO ()
registerSubscription store sidValue subjectValue =
  register
    store
    sidValue
    (SubscriptionMeta subjectValue Nothing StandardSubscription)
    (SubscribeConfig Nothing Nothing)
    (const (pure ()))

message :: SID -> Payload -> Msg
message sidValue payloadValue =
  Msg "A" sidValue Nothing (Just payloadValue) Nothing
