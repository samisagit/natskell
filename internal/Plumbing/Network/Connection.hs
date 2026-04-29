module Network.Connection
  ( connectionApi
  ) where

import qualified Network.Connection.Core as Core
import qualified Network.Connection.Tcp  as Tcp
import qualified Network.Connection.Tls  as Tls
import           Network.ConnectionAPI

connectionApi :: ConnectionAPI
connectionApi =
  ConnectionAPI
    { newConn = Core.newConn
    , reader = ReaderAPI
        { readData = Core.readData
        , closeReader = Core.closeReader
        , openReader = Core.openReader
        }
    , writer = WriterAPI
        { writeData = Core.writeData
        , writeDataLazy = Core.writeDataLazy
        , closeWriter = Core.closeWriter
        , openWriter = Core.openWriter
        }
    , open = Core.openConn
    , close = Core.closeConn
    , connectTcp = Tcp.connectTcp
    , configure = Tls.configureTransport
    }
