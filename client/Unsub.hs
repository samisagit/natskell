module Unsub where

import           Nats.Nats
import           Types
import           Types.Unsub

unsub :: NatsConn a => NatsAPI a -> SID -> Subject -> IO ()
unsub nats sid subject = do
  removeSubscription nats sid
  prepareSend nats
  sendBytes nats $ Unsub subject Nothing

