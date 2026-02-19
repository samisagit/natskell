module Client.Types
  ( Client (..)
  , SubscriptionHeapItem (..)
  , SubscriptionHeap
  , SubscriptionState (..)
  , emptySubscriptionState
  , ConfigState (..)
  , initialConfigState
  , LifecycleState (..)
  ) where

import           Control.Concurrent.STM   (TQueue, TVar)
import           Data.Heap                (MinHeap, empty)
import           Data.Map                 (Map)
import           Data.Time.Clock          (UTCTime)
import           Lib.Logger               (LogContext)
import           Network.Connection       (Conn)
import qualified Nuid
import           Options                  (ClientExitReason (..), Config)
import           Queue.TransactionalQueue (Q, QueueItem)
import           Sid                      (SIDCounter)
import           Types                    (SID)
import qualified Types.Info               as I
import qualified Types.Msg                as M

data SubscriptionHeapItem = SubscriptionHeapItem
                              { sid'   :: SID
                              , expiry :: UTCTime
                              }

instance Eq SubscriptionHeapItem where
  a == b = sid' a == sid' b && expiry a == expiry b

instance Ord SubscriptionHeapItem where
  a `compare` b = expiry a `compare` expiry b

type SubscriptionHeap = MinHeap SubscriptionHeapItem

data SubscriptionState = SubscriptionState
                           { subscriptionCallbacks :: Map SID (Maybe M.Msg -> IO ())
                           , subscriptionExpiries :: SubscriptionHeap
                           }

emptySubscriptionState :: SubscriptionState
emptySubscriptionState = SubscriptionState mempty empty

data ConfigState = ConfigState
                     { cfgConfig     :: Config
                     , cfgServerInfo :: Maybe I.Info
                     }

initialConfigState :: Config -> ConfigState
initialConfigState cfg = ConfigState cfg Nothing

data LifecycleState = Running
                    | Closing ClientExitReason
                    | Closed ClientExitReason

-- | Client is used to interact with the NATS server.
data Client = Client'
                { queue               :: Q QueueItem
                , subscriptions       :: TVar SubscriptionState
                , pings               :: TQueue (IO ())
                , sidCounter          :: TVar SIDCounter
                , inboxNuid           :: TVar Nuid.Nuid
                , configState         :: TVar ConfigState
                , connectionAttempts' :: TVar Int
                , lifecycle           :: TVar LifecycleState
                , conn                :: Conn
                , logContext          :: TVar LogContext
                }
