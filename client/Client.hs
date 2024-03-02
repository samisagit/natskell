module Client where

import           Nats.Nats
import           StrictLock

data Client a = Client
  {
    conn :: NatsAPI a,
    strictLock :: Maybe StrictLock
  }

newClient :: NatsConn a => NatsAPI a -> Bool -> IO (Client a)
newClient nats strict = do
  sl <- newStrictLock
  let resSl = if strict then Just sl else Nothing

  return Client{
  conn = nats,
  strictLock = resSl
  }

