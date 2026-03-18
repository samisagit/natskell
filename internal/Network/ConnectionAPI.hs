module Network.ConnectionAPI
  ( Conn
  , Transport
  , TransportOption (..)
  , ConnectionAPI (..)
  ) where

import           Network.Connection.Types
    ( Conn
    , Transport
    , TransportOption (..)
    )

-- | API wrapper for connection capabilities.
data ConnectionAPI = ConnectionAPI
                       { connectionNew :: IO Conn
                       , connectionOpenTcpTransport :: String -> Int -> IO (Either String Transport)
                       , connectionPointTransport :: Conn -> Transport -> IO ()
                       , connectionConfigure :: Conn -> TransportOption -> IO (Either String ())
                       }
