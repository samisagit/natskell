module Network.ConnectionAPI
  ( ReadError
  , WriteError
  , ReaderAPI (..)
  , WriterAPI (..)
  , Conn
  , TransportOption (..)
  , ConnectionAPI (..)
  , connectionApi
  ) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Lazy    as LBS
import           Network.Connection.Core
    ( Conn
    , ReadError
    , TransportOption (..)
    , WriteError
    )
import qualified Network.Connection.Core as Core
import qualified Network.Connection.Tcp  as Tcp
import qualified Network.Connection.Tls  as Tls

data ReaderAPI reader = ReaderAPI
                          { readerReadData :: reader -> Int -> IO (Either ReadError ByteString)
                          , readerClose :: reader -> IO ()
                          , readerOpen :: reader -> IO ()
                          }

data WriterAPI writer = WriterAPI
                          { writerWriteData :: writer -> ByteString -> IO (Either WriteError ())
                          , writerWriteDataLazy :: writer -> LBS.ByteString -> IO (Either WriteError ())
                          , writerClose :: writer -> IO ()
                          , writerOpen :: writer -> IO ()
                          }

data ConnectionAPI = ConnectionAPI
                       { connectionNew :: IO Conn
                       , connectionReaderApi :: ReaderAPI Conn
                       , connectionWriterApi :: WriterAPI Conn
                       , connectionOpen :: Conn -> IO ()
                       , connectionClose :: Conn -> IO ()
                       , connectionConnectTcp :: Conn -> String -> Int -> IO (Either String ())
                       , connectionConfigure :: Conn -> TransportOption -> IO (Either String ())
                       }

connectionApi :: ConnectionAPI
connectionApi =
  ConnectionAPI
    { connectionNew = Core.newConn
    , connectionReaderApi = ReaderAPI
        { readerReadData = Core.readData
        , readerClose = Core.closeReader
        , readerOpen = Core.openReader
        }
    , connectionWriterApi = WriterAPI
        { writerWriteData = Core.writeData
        , writerWriteDataLazy = Core.writeDataLazy
        , writerClose = Core.closeWriter
        , writerOpen = Core.openWriter
        }
    , connectionOpen = Core.openConn
    , connectionClose = Core.closeConn
    , connectionConnectTcp = Tcp.connectTcp
    , connectionConfigure = Tls.configureTransport
    }
