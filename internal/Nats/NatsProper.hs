module Nats.NatsProper where

import           Data.ByteString
import           Nats.Nats
import qualified Network.Simple.TCP as TCP

instance NatsConn TCP.Socket where
  recv = TCP.recv
  send = TCP.send

connect :: String -> Int -> (() -> IO ByteString) -> IO (NatsAPI TCP.Socket)
connect host port sid = do
  (sock, _) <- TCP.connectSock host $ show port
  nats sock sid
