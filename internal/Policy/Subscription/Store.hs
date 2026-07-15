module Subscription.Store
  ( SubscriptionStore
  , DispatchResult (..)
  , newSubscriptionStore
  , register
  , registerWithAcceptance
  , registerWithDispatchHooks
  , unregister
  , dispatchMessage
  , active
  , closeStore
  , startWorkers
  , awaitCallbackDrain
  , enqueueControl
  , startExpiryWorker
  , awaitNoTrackedExpiries
  , hasTrackedExpiries
  ) where

import           Control.Concurrent
    ( ThreadId
    , forkIOWithUnmask
    , threadDelay
    )
import           Control.Concurrent.STM
import           Control.Exception      (SomeException, finally)
import           Control.Monad          (unless, when)
import qualified Data.Heap              as Heap
import qualified Data.Map               as Map
import           Data.Time.Clock
import           Lib.WorkerPool         (startWorkerPool)
import           Subscription.Types
import qualified Types.Msg              as M
import           Types.Msg              (SID)

data SubscriptionState = SubscriptionState
                           { subscriptionCallbacks :: Map.Map SID SubscriptionCallback
                           , subscriptionExpiryHeap :: Heap.MinHeap (UTCTime, SID)
                           , subscriptionTrackedExpiries :: Map.Map SID UTCTime
                           , subscriptionMeta :: Map.Map SID SubscriptionMeta
                           }

data SubscriptionCallback = SubscriptionCallback
                              { deliverMessage :: Maybe M.Msg -> IO ()
                              , dropMessage    :: IO ()
                              , acceptMessage  :: STM ()
                              , rejectMessage  :: STM ()
                              }

data DispatchResult = DispatchMissing
                    | DispatchQueued
                    | DispatchDropped Bool
  deriving (Eq, Show)

data SubscriptionStore = SubscriptionStore
                           { storeState           :: TVar SubscriptionState
                           , storeCallbackQueue   :: TQueue (IO ())
                           , storeCallbackPending :: TVar Int
                           , storeDeliveryPending :: TVar Int
                           , storeDeliveryBytes   :: TVar Int
                           , storeSlowConsumer    :: TVar Bool
                           , storePendingLimits   :: PendingLimits
                           , storeSlowAction      :: IO ()
                           }

newSubscriptionStore :: PendingLimits -> IO () -> IO SubscriptionStore
newSubscriptionStore limits slowAction =
  SubscriptionStore
    <$> newTVarIO emptySubscriptionState
    <*> newTQueueIO
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO 0
    <*> newTVarIO False
    <*> pure (normalizePendingLimits limits)
    <*> pure slowAction

emptySubscriptionState :: SubscriptionState
emptySubscriptionState =
  SubscriptionState Map.empty Heap.empty Map.empty Map.empty

register :: SubscriptionStore -> SID -> SubscriptionMeta -> SubscribeConfig -> (Maybe M.Msg -> IO ()) -> IO () -> IO ()
register store sid meta cfg =
  registerWithAcceptance store sid meta cfg (pure ())

registerWithAcceptance
  :: SubscriptionStore
  -> SID
  -> SubscriptionMeta
  -> SubscribeConfig
  -> STM ()
  -> (Maybe M.Msg -> IO ())
  -> IO ()
  -> IO ()
registerWithAcceptance store sid meta cfg onAccepted callback onDropped = do
  registerWithDispatchHooks
    store
    sid
    meta
    cfg
    onAccepted
    (pure ())
    callback
    onDropped

registerWithDispatchHooks
  :: SubscriptionStore
  -> SID
  -> SubscriptionMeta
  -> SubscribeConfig
  -> STM ()
  -> STM ()
  -> (Maybe M.Msg -> IO ())
  -> IO ()
  -> IO ()
registerWithDispatchHooks store sid meta cfg onAccepted onRejected callback onDropped = do
  expiryAt <- case expiry cfg of
    Nothing  -> pure Nothing
    Just ttl -> Just . addUTCTime ttl <$> getCurrentTime
  atomically . modifyTVar' (storeState store) $ \state ->
    let stateWithCallback =
          state
            { subscriptionCallbacks =
                Map.insert
                  sid
                  (SubscriptionCallback callback onDropped onAccepted onRejected)
                  (subscriptionCallbacks state)
            , subscriptionMeta = Map.insert sid meta (subscriptionMeta state)
            }
    in case expiryAt of
        Just expiresAt | tracksSubscriptionExpiry meta ->
          trackSubscriptionExpiry sid expiresAt stateWithCallback
        _ ->
          stateWithCallback

unregister :: SubscriptionStore -> SID -> IO ()
unregister store sid =
  atomically $
    modifyTVar' (storeState store) (removeSubscriptionLocal sid)

dispatchMessage :: SubscriptionStore -> M.Msg -> IO DispatchResult
dispatchMessage store msg = do
  (result, onDropped) <- atomically $ do
    state <- readTVar (storeState store)
    let sid = M.sid msg
    case Map.lookup sid (subscriptionCallbacks state) of
      Nothing ->
        pure (DispatchMissing, pure ())
      Just callback -> do
        acceptMessage callback
        let shouldRemove =
              maybe False isOneShotSubscription (Map.lookup sid (subscriptionMeta state))
            nextState =
              if shouldRemove
                then removeSubscriptionLocal sid state
                else state
            messageBytes = M.messageContentSize (M.headers msg) (M.payload msg)
        canQueue <- hasDeliveryCapacity store messageBytes
        writeTVar (storeState store) nextState
        if canQueue
          then do
            enqueueDeliverySTM store messageBytes (deliverMessage callback (Just msg))
            pure (DispatchQueued, pure ())
          else do
            rejectMessage callback
            alreadySlow <- readTVar (storeSlowConsumer store)
            unless alreadySlow $ do
              writeTVar (storeSlowConsumer store) True
              enqueueControlCallbackSTM store (storeSlowAction store)
            pure (DispatchDropped (not alreadySlow), dropMessage callback)
  onDropped
  pure result

active :: SubscriptionStore -> IO [(SID, SubscriptionMeta)]
active store =
  Map.toList . subscriptionMeta <$> readTVarIO (storeState store)

closeStore :: SubscriptionStore -> IO ()
closeStore store =
  atomically (writeTVar (storeState store) emptySubscriptionState)

startWorkers :: Int -> SubscriptionStore -> STM () -> (SomeException -> IO ()) -> IO [ThreadId]
startWorkers concurrency store =
  startWorkerPool (max 1 concurrency) (storeCallbackQueue store)

awaitCallbackDrain :: SubscriptionStore -> STM ()
awaitCallbackDrain store = do
  pending <- readTVar (storeCallbackPending store)
  check (pending == 0)

enqueueControl :: SubscriptionStore -> IO () -> IO ()
enqueueControl store = atomically . enqueueControlCallbackSTM store

startExpiryWorker :: SubscriptionStore -> IO Bool -> IO ThreadId
startExpiryWorker store shouldStop =
  forkIOWithUnmask (\unmask -> unmask loop)
  where
    loop = do
      stop <- shouldStop
      unless stop $ do
        now <- getCurrentTime
        expiredAny <- expireReadySubscriptions store now
        unless expiredAny (threadDelay 1000000)
        loop

awaitNoTrackedExpiries :: SubscriptionStore -> STM ()
awaitNoTrackedExpiries store = do
  tracked <- subscriptionTrackedExpiries <$> readTVar (storeState store)
  unless (Map.null tracked) retry

hasTrackedExpiries :: SubscriptionStore -> IO Bool
hasTrackedExpiries store =
  not . Map.null . subscriptionTrackedExpiries <$> readTVarIO (storeState store)

enqueueControlCallbackSTM :: SubscriptionStore -> IO () -> STM ()
enqueueControlCallbackSTM store action = do
  modifyTVar' (storeCallbackPending store) (+1)
  let wrapped =
        action `finally` atomically (modifyTVar' (storeCallbackPending store) (subtract 1))
  writeTQueue (storeCallbackQueue store) wrapped

enqueueDeliverySTM :: SubscriptionStore -> Int -> IO () -> STM ()
enqueueDeliverySTM store messageBytes action = do
  modifyTVar' (storeCallbackPending store) (+1)
  modifyTVar' (storeDeliveryPending store) (+1)
  modifyTVar' (storeDeliveryBytes store) (+ messageBytes)
  let wrapped =
        action `finally` atomically (releaseDeliverySTM store messageBytes)
  writeTQueue (storeCallbackQueue store) wrapped

releaseDeliverySTM :: SubscriptionStore -> Int -> STM ()
releaseDeliverySTM store messageBytes = do
  modifyTVar' (storeCallbackPending store) (subtract 1)
  modifyTVar' (storeDeliveryPending store) (subtract 1)
  modifyTVar' (storeDeliveryBytes store) (subtract messageBytes)
  pendingMessages <- readTVar (storeDeliveryPending store)
  pendingBytes <- readTVar (storeDeliveryBytes store)
  let limits = storePendingLimits store
      belowLowWater =
        pendingMessages <= pendingMessageLimit limits `div` 2
          && pendingBytes <= pendingByteLimit limits `div` 2
  when belowLowWater $
    writeTVar (storeSlowConsumer store) False

hasDeliveryCapacity :: SubscriptionStore -> Int -> STM Bool
hasDeliveryCapacity store messageBytes = do
  pendingMessages <- readTVar (storeDeliveryPending store)
  pendingBytes <- readTVar (storeDeliveryBytes store)
  let limits = storePendingLimits store
  pure $
    pendingMessages < pendingMessageLimit limits
      && messageBytes <= pendingByteLimit limits - pendingBytes

expireReadySubscriptions :: SubscriptionStore -> UTCTime -> IO Bool
expireReadySubscriptions store now =
  atomically $ do
    state <- readTVar (storeState store)
    let (actions, nextState) = collectExpiredCallbacks now state
    writeTVar (storeState store) nextState
    mapM_ (enqueueControlCallbackSTM store) actions
    pure (not (null actions))

trackSubscriptionExpiry :: SID -> UTCTime -> SubscriptionState -> SubscriptionState
trackSubscriptionExpiry sid expiry state =
  state
    { subscriptionExpiryHeap = Heap.insert (expiry, sid) (subscriptionExpiryHeap state)
    , subscriptionTrackedExpiries = Map.insert sid expiry (subscriptionTrackedExpiries state)
    }

removeSubscriptionLocal :: SID -> SubscriptionState -> SubscriptionState
removeSubscriptionLocal sid state =
  state
    { subscriptionCallbacks = Map.delete sid (subscriptionCallbacks state)
    , subscriptionTrackedExpiries = Map.delete sid (subscriptionTrackedExpiries state)
    , subscriptionMeta = Map.delete sid (subscriptionMeta state)
    }

collectExpiredCallbacks :: UTCTime -> SubscriptionState -> ([IO ()], SubscriptionState)
collectExpiredCallbacks now = go []
  where
    go actions state =
      case Heap.viewHead (subscriptionExpiryHeap state) of
        Nothing ->
          (reverse actions, state)
        Just (expiry, sid) ->
          case Map.lookup sid (subscriptionTrackedExpiries state) of
            Nothing ->
              go actions (dropHeapHead state)
            Just trackedExpiry
              | trackedExpiry /= expiry ->
                  go actions (dropHeapHead state)
              | now < trackedExpiry ->
                  (reverse actions, state)
              | otherwise ->
                  let callback = Map.lookup sid (subscriptionCallbacks state)
                      state' = removeSubscriptionLocal sid (dropHeapHead state)
                      actions' =
                        maybe
                          actions
                          ((: actions) . ($ Nothing) . deliverMessage)
                          callback
                  in go actions' state'

dropHeapHead :: SubscriptionState -> SubscriptionState
dropHeapHead state =
  case Heap.view (subscriptionExpiryHeap state) of
    Nothing            -> state
    Just (_, heapTail) -> state { subscriptionExpiryHeap = heapTail }

normalizePendingLimits :: PendingLimits -> PendingLimits
normalizePendingLimits limits =
  limits
    { pendingMessageLimit = max 1 (pendingMessageLimit limits)
    , pendingByteLimit = max 1 (pendingByteLimit limits)
    }
