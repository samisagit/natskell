module Unsub (unsub) where

import           Client
import           Nats.Nats
import           Types
import           Types.Unsub

unsub :: NatsConn a => Client a -> SID -> IO ()
unsub (Client conn _) sid = do
  removeSubscription conn sid
  sendBytes conn $ Unsub sid Nothing

