{-# LANGUAGE OverloadedStrings #-}

module Client.Subscription
  ( awaitSubscriptionGC
  , cancelExpiredSubscriptions
  , msgRouterM
  , nextInbox
  , nextSid
  , subscribeInternal
  , unsubscribeInternal
  ) where

import           Client.Runtime           (runClient, writeToClientQueue)
import           Client.Types
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString          as BS
import           Data.Heap                (insert, view, viewHead)
import qualified Data.Map                 as Map
import           Data.Maybe               (isJust)
import           Data.Time.Clock
import           Lib.CallOption
import           Lib.Logger               (AppM, LogLevel (..), logMessage)
import           MSGView                  (transformMsg)
import qualified Nuid
import           Options
import           Queue.TransactionalQueue (QueueItem (..))
import           Sid                      (nextSID)
import           Types
import qualified Types.Msg                as M
import qualified Types.Sub                as Sub
import qualified Types.Unsub              as Unsub

subscribeInternal :: Bool -> Client -> Subject -> [SubscribeOpts] -> (Maybe MsgView -> IO ()) -> IO SID
subscribeInternal isReply client subject opts callback = do
  runClient client . logMessage Debug $ ("subscribing to subject: " ++ show subject)
  sid <- nextSid client
  let subConfig = applyCallOptions opts defaultSubscribeConfig
  let cb = if isReply
        then \m -> do
          callback (fmap transformMsg m)
          unsubscribeInternal client sid
        else callback . fmap transformMsg
  expiry <- case subscriptionExpiry subConfig of
    Nothing          -> pure Nothing
    Just replyExpiry -> Just . addUTCTime replyExpiry <$> getCurrentTime
  atomically $ do
    modifyTVar' (subscriptions client) $ \subs ->
      let callbacks' = Map.insert sid cb (subscriptionCallbacks subs)
          expiries' = if isReply
            then case expiry of
              Just expiryValue -> insert (SubscriptionHeapItem sid expiryValue) (subscriptionExpiries subs)
              Nothing -> subscriptionExpiries subs
            else subscriptionExpiries subs
      in subs { subscriptionCallbacks = callbacks', subscriptionExpiries = expiries' }
  let sub = Sub.Sub
        { Sub.subject = subject
        , Sub.queueGroup = Nothing
        , Sub.sid = sid
        }
  writeToClientQueue client (QueueItem sub)
  return sid

unsubscribeInternal :: Client -> SID -> IO ()
unsubscribeInternal client sid = do
  runClient client . logMessage Debug $ ("unsubscribing SID: " ++ show sid)
  atomically $ modifyTVar' (subscriptions client) (\s -> s { subscriptionCallbacks = Map.delete sid (subscriptionCallbacks s) })
  let unsub = Unsub.Unsub
        { Unsub.sid = sid
        , Unsub.maxMsg = Nothing
        }
  writeToClientQueue client (QueueItem unsub)

cancelExpiredSubscriptions :: Client -> IO ()
cancelExpiredSubscriptions client = do
  runClient client $ logMessage Debug "running subscription GC"
  now <- getCurrentTime
  action <- atomically $ do
    state <- readTVar (subscriptions client)
    let viewed = viewHead (subscriptionExpiries state)
    case viewed of
      Nothing -> retry
      Just item ->
        if now < expiry item
          then return Nothing
          else do
            case view (subscriptionExpiries state) of
              Nothing -> return Nothing
              Just (_, heapTail) -> do
                let callback = Map.lookup (sid' item) (subscriptionCallbacks state)
                    newCallbacks = maybe (subscriptionCallbacks state) (const (Map.delete (sid' item) (subscriptionCallbacks state))) callback
                    newState = state { subscriptionCallbacks = newCallbacks, subscriptionExpiries = heapTail }
                writeTVar (subscriptions client) newState
                return callback
  runClient client $ case action of
    Just a -> liftIO $ a Nothing
    Nothing  -> do
      liftIO $ threadDelay 1000000 -- 1 second
      return ()

awaitSubscriptionGC :: Client -> IO ()
awaitSubscriptionGC client = do
  atomically $ do
    subs <- readTVar (subscriptions client)
    when (isJust (viewHead (subscriptionExpiries subs))) retry

msgRouterM :: Client -> M.Msg -> AppM ()
msgRouterM client msg = do
  let sid = M.sid msg
  callbacks <- liftIO $ subscriptionCallbacks <$> readTVarIO (subscriptions client)
  case Map.lookup sid callbacks of
    Just callback -> do
      logMessage Debug $ "running callback for SID: " ++ show sid
      liftIO . callback $ Just msg
    Nothing       -> logMessage Error $ "callback missing for SID: " ++ show sid

nextSid :: Client -> IO SID
nextSid client = atomically $ do
  counter <- readTVar (sidCounter client)
  let (sid, counter') = nextSID counter
  writeTVar (sidCounter client) counter'
  return sid

nextInbox :: Client -> IO Subject
nextInbox client = atomically $ do
  nuid <- readTVar (inboxNuid client)
  let (token, nuid') = Nuid.nextNuid nuid
      inbox = BS.append inboxPrefix token
  writeTVar (inboxNuid client) nuid'
  return inbox

inboxPrefix :: Subject
inboxPrefix = "_INBOX."
