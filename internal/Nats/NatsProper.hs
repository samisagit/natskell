module Nats.NatsProper where

import           Nats.Nats
import qualified Network.Simple.TCP as TCP

instance NatsConn TCP.Socket where
  recv = TCP.recv
  send = TCP.send

connect :: String -> Int -> IO (NatsAPI TCP.Socket)
connect host port = do
  (sock, _) <- TCP.connectSock host $ show port
  nats sock
