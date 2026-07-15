module Network.Connection.Tcp
  ( connectTcp
  ) where

import           Control.Exception         (displayException, mask, onException)
import qualified Data.ByteString.Lazy      as LBS
import           Lib.Exception             (trySync)
import           Network.Connection.Core   (pointTransport)
import qualified Network.Connection.Tls    as Tls
import           Network.Connection.Types  (Conn, Transport (..))
import qualified Network.Simple.TCP        as TCP
import qualified Network.Socket            as NS
import qualified Network.Socket.ByteString as NSB

tcpTransport :: NS.Socket -> Transport
tcpTransport sock =
  Transport
    { transportRead = NSB.recv sock
    , transportWrite = NSB.sendAll sock
    , transportWriteLazy = NSB.sendMany sock . LBS.toChunks
    , transportFlush = pure ()
    , transportClose = NS.close sock
    , transportAbort = NS.close sock
    , transportUpgrade = Just (Tls.upgradeTcp sock)
    }

connectTcp :: Conn -> String -> Int -> IO (Either String ())
connectTcp conn host port = do
  result <- trySync (mask $ \restore -> do
    (sock, _) <- TCP.connectSock host (show port)
    let transport = tcpTransport sock
    restore
      (do
        NS.setSocketOption sock NS.NoDelay 1
        NS.setSocketOption sock NS.Cork 0)
      `onException` NS.close sock
    pointTransport conn transport `onException` transportClose transport)
  pure $
    case result of
      Left err -> Left (displayException err)
      Right () -> Right ()
