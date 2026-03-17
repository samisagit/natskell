module Client.SubscriptionAPI
  ( SubscribeConfig (..)
  , SubscriptionGC
  , SubscriptionGCAction (..)
  , emptySubscriptionGC
  , trackSubscriptionExpiry
  , cancelSubscriptionExpiry
  , collectExpiredSubscription
  , hasTrackedSubscriptionExpiries
  , SubscriptionState (..)
  , SubscriptionMeta (..)
  ) where

import qualified Data.Heap       as Heap
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import           Types           (SID, Subject)
import qualified Types.Msg       as M

newtype SubscribeConfig = SubscribeConfig { subscriptionExpiry :: Maybe NominalDiffTime }

data SubscriptionExpiry = SubscriptionExpiry
                            { expirySid :: SID
                            , expiryAt  :: UTCTime
                            }

instance Eq SubscriptionExpiry where
  a == b = expirySid a == expirySid b && expiryAt a == expiryAt b

instance Ord SubscriptionExpiry where
  a `compare` b = expiryAt a `compare` expiryAt b

data SubscriptionGC = SubscriptionGC
                        { gcHeap    :: Heap.MinHeap SubscriptionExpiry
                        , gcTracked :: Map SID UTCTime
                        }

data SubscriptionGCAction = NoTrackedSubscriptionExpiries SubscriptionGC
                          | AwaitingSubscriptionExpiry UTCTime SubscriptionGC
                          | ExpireSubscription SID SubscriptionGC

emptySubscriptionGC :: SubscriptionGC
emptySubscriptionGC = SubscriptionGC Heap.empty Map.empty

trackSubscriptionExpiry :: SID -> UTCTime -> SubscriptionGC -> SubscriptionGC
trackSubscriptionExpiry sid expiry gc =
  gc
    { gcHeap = Heap.insert (SubscriptionExpiry sid expiry) (gcHeap gc)
    , gcTracked = Map.insert sid expiry (gcTracked gc)
    }

cancelSubscriptionExpiry :: SID -> SubscriptionGC -> SubscriptionGC
cancelSubscriptionExpiry sid gc =
  gc { gcTracked = Map.delete sid (gcTracked gc) }

collectExpiredSubscription :: UTCTime -> SubscriptionGC -> SubscriptionGCAction
collectExpiredSubscription now = go
  where
    go gc =
      case Heap.viewHead (gcHeap gc) of
        Nothing -> NoTrackedSubscriptionExpiries gc
        Just headExpiry ->
          case Map.lookup (expirySid headExpiry) (gcTracked gc) of
            Nothing ->
              go (dropHead gc)
            Just trackedExpiry
              | trackedExpiry /= expiryAt headExpiry ->
                  go (dropHead gc)
              | now < trackedExpiry ->
                  AwaitingSubscriptionExpiry trackedExpiry gc
              | otherwise ->
                  let sid = expirySid headExpiry
                      gc' = cancelSubscriptionExpiry sid (dropHead gc)
                  in ExpireSubscription sid gc'

    dropHead gc =
      case Heap.view (gcHeap gc) of
        Nothing            -> gc
        Just (_, heapTail) -> gc { gcHeap = heapTail }

hasTrackedSubscriptionExpiries :: SubscriptionGC -> Bool
hasTrackedSubscriptionExpiries = not . Map.null . gcTracked

data SubscriptionState = SubscriptionState
                           { subscriptionCallbacks :: Map SID (Maybe M.Msg -> IO ())
                           , subscriptionGC :: SubscriptionGC
                           , subscriptionMeta :: Map SID SubscriptionMeta
                           }

data SubscriptionMeta = SubscriptionMeta
                          { subscriptionSubject    :: Subject
                          , subscriptionQueueGroup :: Maybe Subject
                          , subscriptionIsReply    :: Bool
                          }
