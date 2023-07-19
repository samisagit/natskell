module Unsub where

import           Types.Unsub
import           Nats.Nats
import Types

unsub :: NatsConn a => NatsAPI a -> SID -> Subject -> IO ()
unsub nats sid subject = do
  removeSubscription nats sid
  sendBytes nats $ Unsub subject Nothing

