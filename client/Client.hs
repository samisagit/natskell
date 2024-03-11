module Client (Client(..), newClient, withAckCallback) where

import           Nats.Nats

type ClientOption a = Client a -> Client a

applyClientOptions :: Client a -> [ClientOption a] -> Client a
applyClientOptions = foldl (flip ($))

data Client a = Client
  {
    conn        :: NatsAPI a,
    ackCallback :: Maybe (IO ())
  }

withAckCallback :: IO () -> ClientOption a
withAckCallback cb c = Client{
    conn = conn c,
    ackCallback = Just cb
  }

newClient :: NatsConn a => NatsAPI a -> [ClientOption a] -> IO (Client a)
newClient nats opts = do
  let defaultClient = Client{
  conn = nats,
  ackCallback = Nothing
  }

  return $ applyClientOptions defaultClient opts

