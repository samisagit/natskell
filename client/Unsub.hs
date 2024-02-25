module Unsub where

import           Nats.Nats
import           Types
import           Types.Unsub

unsub :: NatsConn a => NatsAPI a -> SID -> IO ()
unsub nats sid = do
  removeSubscription nats sid
  prepareSend nats
  sendBytes nats $ Unsub sid Nothing

