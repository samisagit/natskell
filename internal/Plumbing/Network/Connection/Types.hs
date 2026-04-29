module Network.Connection.Types
  ( ReadError
  , WriteError
  , Transport (..)
  , TransportOption (..)
  , Conn (..)
  ) where

import           Control.Concurrent.STM
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as LBS
import qualified Network.TLS            as TLS

type ReadError = String
type WriteError = String

data Transport = Transport
                   { transportRead :: Int -> IO ByteString
                   , transportWrite :: ByteString -> IO ()
                   , transportWriteLazy :: LBS.ByteString -> IO ()
                   , transportFlush :: IO ()
                   , transportClose :: IO ()
                   , transportUpgrade :: Maybe (TLS.ClientParams -> IO (Either String Transport))
                   }

data TransportOption = TransportOption
                         { transportHost :: String
                         , transportTlsRequired :: Bool
                         , transportTlsRequested :: Bool
                         , transportTlsCert :: Maybe (ByteString, ByteString)
                         , transportInitialBytes :: ByteString
                         }

data Conn = Conn
              { transport         :: TMVar Transport
              , readBlock         :: TMVar ()
              , writeBlock        :: TMVar ()
              , readBuffer        :: TVar ByteString
              , readQueue         :: TBQueue (Either ReadError ByteString)
              , readWorkerRunning :: TVar Bool
              , readWorkerEnabled :: TVar Bool
              }
