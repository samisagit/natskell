{-# LANGUAGE OverloadedStrings #-}

module Client.Subscription
  ( awaitSubscriptionGC
  , cancelExpiredSubscriptions
  , msgRouterM
  , nextInbox
  , nextSid
  , resubscribeAll
  , subscribeInternal
  , unsubscribeInternal
  ) where

import           Client.CallbacksAPI         (CallbacksAPI (callbacksEnqueue))
import           Client.RuntimeAPI
    ( ClientState (..)
    , RuntimeAPI (runtimeRunClient, runtimeWriteToClientQueue)
    )
import           Client.SubscriptionAPI
    ( SubscribeConfig (..)
    , SubscriptionGCAction (..)
    , SubscriptionMeta (..)
    , SubscriptionState (..)
    , cancelSubscriptionExpiry
    , collectExpiredSubscription
    , hasTrackedSubscriptionExpiries
    , trackSubscriptionExpiry
    )
import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString             as BS
import qualified Data.Map                    as Map
import           Data.Time.Clock
import           Lib.Logger
    ( AppM
    , LogLevel (..)
    , MonadLogger (..)
    )
import           NuidAPI                     (NuidAPI (nuidNext))
import           Queue.TransactionalQueueAPI (QueueItem (..))
import           SidAPI                      (SidAPI (sidNext))
import           Transformers.Transformers   ()
import           Types
import qualified Types.Msg                   as M
import qualified Types.Sub                   as Sub
import qualified Types.Unsub                 as Unsub

subscribeInternal :: RuntimeAPI -> SidAPI -> Bool -> ClientState -> Subject -> SubscribeConfig -> (Maybe M.Msg -> IO ()) -> IO SID
subscribeInternal runtimeApi sidApi isReply client subject subConfig callback = do
  runtimeRunClient runtimeApi client . logMessage Debug $ ("subscribing to subject: " ++ show subject)
  sid <- nextSid sidApi client
  let cb = if isReply
        then \m -> do
          callback m
          removeSubscriptionLocal client sid
        else callback
  expiry <- case subscriptionExpiry subConfig of
    Nothing          -> pure Nothing
    Just replyExpiry -> Just . addUTCTime replyExpiry <$> getCurrentTime
  atomically $ do
    modifyTVar' (subscriptions client) $ \subs ->
      let callbacks' = Map.insert sid cb (subscriptionCallbacks subs)
          meta = SubscriptionMeta
            { subscriptionSubject = subject
            , subscriptionQueueGroup = Nothing
            , subscriptionIsReply = isReply
            }
          meta' = Map.insert sid meta (subscriptionMeta subs)
          gc' = if isReply
            then case expiry of
              Just expiryValue -> trackSubscriptionExpiry sid expiryValue (subscriptionGC subs)
              Nothing -> subscriptionGC subs
            else subscriptionGC subs
      in subs { subscriptionCallbacks = callbacks', subscriptionGC = gc', subscriptionMeta = meta' }
  let sub = Sub.Sub
        { Sub.subject = subject
        , Sub.queueGroup = Nothing
        , Sub.sid = sid
        }
  runtimeWriteToClientQueue runtimeApi client (QueueItem sub)
  when isReply $ do
    let unsub = Unsub.Unsub
          { Unsub.sid = sid
          , Unsub.maxMsg = Just 1
          }
    runtimeWriteToClientQueue runtimeApi client (QueueItem unsub)
  return sid

unsubscribeInternal :: RuntimeAPI -> ClientState -> SID -> IO ()
unsubscribeInternal runtimeApi client sid = do
  runtimeRunClient runtimeApi client . logMessage Debug $ ("unsubscribing SID: " ++ show sid)
  removeSubscriptionLocal client sid
  let unsub = Unsub.Unsub
        { Unsub.sid = sid
        , Unsub.maxMsg = Nothing
        }
  runtimeWriteToClientQueue runtimeApi client (QueueItem unsub)

removeSubscriptionLocal :: ClientState -> SID -> IO ()
removeSubscriptionLocal client sid =
  atomically $ modifyTVar' (subscriptions client) (\s -> s
    { subscriptionCallbacks = Map.delete sid (subscriptionCallbacks s)
    , subscriptionGC = cancelSubscriptionExpiry sid (subscriptionGC s)
    , subscriptionMeta = Map.delete sid (subscriptionMeta s)
    })

cancelExpiredSubscriptions :: RuntimeAPI -> CallbacksAPI -> ClientState -> IO ()
cancelExpiredSubscriptions runtimeApi callbacksApi client = do
  runtimeRunClient runtimeApi client $ logMessage Debug "running subscription GC"
  now <- getCurrentTime
  action <- atomically $ do
    state <- readTVar (subscriptions client)
    case collectExpiredSubscription now (subscriptionGC state) of
      NoTrackedSubscriptionExpiries gc -> do
        writeTVar (subscriptions client) (state { subscriptionGC = gc })
        retry
      AwaitingSubscriptionExpiry _ gc -> do
        writeTVar (subscriptions client) (state { subscriptionGC = gc })
        return Nothing
      ExpireSubscription sidValue gc -> do
        let callback = Map.lookup sidValue (subscriptionCallbacks state)
            newCallbacks = Map.delete sidValue (subscriptionCallbacks state)
            newMeta = Map.delete sidValue (subscriptionMeta state)
            newState = state
              { subscriptionCallbacks = newCallbacks
              , subscriptionGC = gc
              , subscriptionMeta = newMeta
              }
        writeTVar (subscriptions client) newState
        return callback
  runtimeRunClient runtimeApi client $ case action of
    Just a -> liftIO $ callbacksEnqueue callbacksApi client (a Nothing)
    Nothing  -> do
      liftIO $ threadDelay 1000000 -- 1 second
      return ()

awaitSubscriptionGC :: ClientState -> IO ()
awaitSubscriptionGC client = do
  atomically $ do
    subs <- readTVar (subscriptions client)
    when (hasTrackedSubscriptionExpiries (subscriptionGC subs)) retry

msgRouterM :: CallbacksAPI -> ClientState -> M.Msg -> AppM ()
msgRouterM callbacksApi client msg = do
  let sid = M.sid msg
  callbacks <- liftIO $ subscriptionCallbacks <$> readTVarIO (subscriptions client)
  case Map.lookup sid callbacks of
    Just callback -> do
      logMessage Debug $ "running callback for SID: " ++ show sid
      liftIO $ callbacksEnqueue callbacksApi client (callback (Just msg))
    Nothing       -> logMessage Error $ "callback missing for SID: " ++ show sid

resubscribeAll :: RuntimeAPI -> ClientState -> IO ()
resubscribeAll runtimeApi client = do
  state <- readTVarIO (subscriptions client)
  let metas = Map.toList (subscriptionMeta state)
      active = filter (not . subscriptionIsReply . snd) metas
  unless (null active) (runtimeRunClient runtimeApi client . logMessage Info $
    "resubscribing " ++ show (length active) ++ " subscriptions")
  forM_ active $ \(sid, meta) -> do
    let sub = Sub.Sub
          { Sub.subject = subscriptionSubject meta
          , Sub.queueGroup = subscriptionQueueGroup meta
          , Sub.sid = sid
          }
    runtimeWriteToClientQueue runtimeApi client (QueueItem sub)

nextSid :: SidAPI -> ClientState -> IO SID
nextSid sidApi client = atomically $ do
  counter <- readTVar (sidCounter client)
  let (sid, counter') = sidNext sidApi counter
  writeTVar (sidCounter client) counter'
  return sid

nextInbox :: NuidAPI -> ClientState -> IO Subject
nextInbox nuidApi client = atomically $ do
  nuid <- readTVar (inboxNuid client)
  let (token, nuid') = nuidNext nuidApi nuid
      inbox = BS.append inboxPrefix token
  writeTVar (inboxNuid client) nuid'
  return inbox

inboxPrefix :: Subject
inboxPrefix = "_INBOX."
