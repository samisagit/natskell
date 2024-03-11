module Unsub where

import           Client
import           Nats.Nats
import           Types
import           Types.Unsub

unsub :: NatsConn a => Client a -> SID -> IO ()
unsub (Client conn sl) sid = do
  removeSubscription conn sid
  sendBytes conn $ Unsub sid Nothing

