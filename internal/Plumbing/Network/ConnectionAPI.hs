module Network.ConnectionAPI
  ( ReadError
  , WriteError
  , ReaderAPI (..)
  , WriterAPI (..)
  , Conn
  , TransportOption (..)
  , ConnectionAPI (..)
  ) where

import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as LBS
import           Network.Connection.Types
    ( Conn
    , ReadError
    , TransportOption (..)
    , WriteError
    )

data ReaderAPI reader = ReaderAPI
                          { readData :: reader -> Int -> IO (Either ReadError ByteString)
                          , closeReader :: reader -> IO ()
                          , openReader :: reader -> IO ()
                          }

data WriterAPI writer = WriterAPI
                          { writeData :: writer -> ByteString -> IO (Either WriteError ())
                          , writeDataLazy :: writer -> LBS.ByteString -> IO (Either WriteError ())
                          , closeWriter :: writer -> IO ()
                          , openWriter :: writer -> IO ()
                          }

data ConnectionAPI = ConnectionAPI
                       { newConn :: IO Conn
                       , reader :: ReaderAPI Conn
                       , writer :: WriterAPI Conn
                       , open :: Conn -> IO ()
                       , close :: Conn -> IO ()
                       , connectTcp :: Conn -> String -> Int -> IO (Either String ())
                       , configure :: Conn -> TransportOption -> IO (Either String ())
                       }
