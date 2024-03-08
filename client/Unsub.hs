module Unsub where

import           Client
import           Nats.Nats
import           StrictLock
import           Types
import           Types.Unsub

unsub :: NatsConn a => Client a -> SID -> IO ()
unsub (Client conn sl) sid = do
  removeSubscription conn sid
  request sl . sendBytes conn $ Unsub sid Nothing

