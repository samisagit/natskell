module Client.SubscriptionAPI
  ( SubscribeConfig (..)
  , SubscriptionAPI (..)
  , SubscriptionState (..)
  , SubscriptionMeta (..)
  ) where

import qualified Data.Heap        as Heap
import           Data.Map         (Map)
import           Data.Time.Clock  (NominalDiffTime, UTCTime)
import           Lib.Logger.Types (AppM)
import qualified Types.Msg        as M
import           Types.Msg        (SID, Subject)

newtype SubscribeConfig = SubscribeConfig { subscriptionExpiry :: Maybe NominalDiffTime }

data SubscriptionAPI runtime callbacks sid nuid client = SubscriptionAPI
                                                           { subscriptionAwaitGC :: client -> IO ()
                                                           , subscriptionCancelExpired :: runtime -> callbacks -> client -> IO ()
                                                           , subscriptionMsgRouterM :: callbacks -> client -> M.Msg -> AppM ()
                                                           , subscriptionNextInbox :: nuid -> client -> IO Subject
                                                           , subscriptionNextSid :: sid -> client -> IO SID
                                                           , subscriptionResubscribeAll :: runtime -> client -> IO ()
                                                           , subscriptionSubscribe :: runtime -> sid -> Bool -> client -> Subject -> SubscribeConfig -> (Maybe M.Msg -> IO ()) -> IO SID
                                                           , subscriptionUnsubscribe :: runtime -> client -> SID -> IO ()
                                                           }

data SubscriptionState = SubscriptionState
                           { subscriptionCallbacks :: Map SID (Maybe M.Msg -> IO ())
                           , subscriptionExpiryHeap :: Heap.MinHeap (UTCTime, SID)
                           , subscriptionTrackedExpiries :: Map SID UTCTime
                           , subscriptionMeta :: Map SID SubscriptionMeta
                           }

data SubscriptionMeta = SubscriptionMeta
                          { subscriptionSubject    :: Subject
                          , subscriptionQueueGroup :: Maybe Subject
                          , subscriptionIsReply    :: Bool
                          }
