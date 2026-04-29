{-# LANGUAGE TypeApplications #-}

module Network.Connection.Tcp
  ( connectTcp
  ) where

import           Control.Exception
import qualified Data.ByteString.Lazy      as LBS
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
    , transportUpgrade = Just (Tls.upgradeTcp sock)
    }

openTcpTransport :: String -> Int -> IO (Either String Transport)
openTcpTransport host port = do
  result <- try @SomeException $ do
    (sock, _) <- TCP.connectSock host (show port)
    NS.setSocketOption sock NS.NoDelay 1
    NS.setSocketOption sock NS.Cork 0
    pure (tcpTransport sock)
  case result of
    Left err        -> return (Left (show err))
    Right transport -> return (Right transport)

connectTcp :: Conn -> String -> Int -> IO (Either String ())
connectTcp conn host port = do
  result <- openTcpTransport host port
  case result of
    Left err -> return (Left err)
    Right transport -> do
      pointTransport conn transport
      return (Right ())
