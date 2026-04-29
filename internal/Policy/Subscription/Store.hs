module Subscription.Store
  ( SubscriptionStore
  , newSubscriptionStore
  , register
  , unregister
  , dispatchMessage
  , active
  , startWorkers
  , awaitCallbackDrain
  , startExpiryWorker
  , awaitNoTrackedExpiries
  , hasTrackedExpiries
  ) where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Exception      (SomeException, finally)
import           Control.Monad          (unless, void)
import qualified Data.Heap              as Heap
import qualified Data.Map               as Map
import           Data.Time.Clock
import           Lib.WorkerPool         (startWorkerPool)
import           Subscription.Types
import qualified Types.Msg              as M
import           Types.Msg              (SID)

data SubscriptionState = SubscriptionState
                           { subscriptionCallbacks :: Map.Map SID (Maybe M.Msg -> IO ())
                           , subscriptionExpiryHeap :: Heap.MinHeap (UTCTime, SID)
                           , subscriptionTrackedExpiries :: Map.Map SID UTCTime
                           , subscriptionMeta :: Map.Map SID SubscriptionMeta
                           }

data SubscriptionStore = SubscriptionStore
                           { storeState           :: TVar SubscriptionState
                           , storeCallbackQueue   :: TQueue (IO ())
                           , storeCallbackPending :: TVar Int
                           }

newSubscriptionStore :: IO SubscriptionStore
newSubscriptionStore =
  SubscriptionStore
    <$> newTVarIO emptySubscriptionState
    <*> newTQueueIO
    <*> newTVarIO 0

emptySubscriptionState :: SubscriptionState
emptySubscriptionState =
  SubscriptionState Map.empty Heap.empty Map.empty Map.empty

register :: SubscriptionStore -> SID -> SubscriptionMeta -> SubscribeConfig -> (Maybe M.Msg -> IO ()) -> IO ()
register store sid meta cfg callback = do
  expiryAt <- case expiry cfg of
    Nothing  -> pure Nothing
    Just ttl -> Just . addUTCTime ttl <$> getCurrentTime
  atomically . modifyTVar' (storeState store) $ \state ->
    let stateWithCallback =
          state
            { subscriptionCallbacks = Map.insert sid callback (subscriptionCallbacks state)
            , subscriptionMeta = Map.insert sid meta (subscriptionMeta state)
            }
    in case expiryAt of
        Just expiresAt | isReply meta ->
          trackSubscriptionExpiry sid expiresAt stateWithCallback
        _ ->
          stateWithCallback

unregister :: SubscriptionStore -> SID -> IO ()
unregister store sid =
  atomically $
    modifyTVar' (storeState store) (removeSubscriptionLocal sid)

dispatchMessage :: SubscriptionStore -> M.Msg -> IO Bool
dispatchMessage store msg =
  atomically $ do
    state <- readTVar (storeState store)
    let sid = M.sid msg
    case Map.lookup sid (subscriptionCallbacks state) of
      Nothing ->
        pure False
      Just callback -> do
        let shouldRemove =
              maybe False isReply (Map.lookup sid (subscriptionMeta state))
            nextState =
              if shouldRemove
                then removeSubscriptionLocal sid state
                else state
        writeTVar (storeState store) nextState
        enqueueCallbackSTM store (callback (Just msg))
        pure True

active :: SubscriptionStore -> IO [(SID, SubscriptionMeta)]
active store =
  Map.toList . subscriptionMeta <$> readTVarIO (storeState store)

startWorkers :: Int -> SubscriptionStore -> STM () -> (SomeException -> IO ()) -> IO ()
startWorkers concurrency store =
  startWorkerPool (max 1 concurrency) (storeCallbackQueue store)

awaitCallbackDrain :: SubscriptionStore -> STM ()
awaitCallbackDrain store = do
  pending <- readTVar (storeCallbackPending store)
  check (pending == 0)

startExpiryWorker :: SubscriptionStore -> IO Bool -> IO ()
startExpiryWorker store shouldStop =
  void . forkIO $ loop
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

enqueueCallbackSTM :: SubscriptionStore -> IO () -> STM ()
enqueueCallbackSTM store action = do
  modifyTVar' (storeCallbackPending store) (+1)
  let wrapped =
        action `finally` atomically (modifyTVar' (storeCallbackPending store) (subtract 1))
  writeTQueue (storeCallbackQueue store) wrapped

expireReadySubscriptions :: SubscriptionStore -> UTCTime -> IO Bool
expireReadySubscriptions store now =
  atomically $ do
    state <- readTVar (storeState store)
    let (actions, nextState) = collectExpiredCallbacks now state
    writeTVar (storeState store) nextState
    mapM_ (enqueueCallbackSTM store) actions
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
                      actions' = maybe actions ((: actions) . ($ Nothing)) callback
                  in go actions' state'

dropHeapHead :: SubscriptionState -> SubscriptionState
dropHeapHead state =
  case Heap.view (subscriptionExpiryHeap state) of
    Nothing            -> state
    Just (_, heapTail) -> state { subscriptionExpiryHeap = heapTail }
