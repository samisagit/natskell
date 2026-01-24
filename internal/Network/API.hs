module Network.API where

import           Data.ByteString

type ReadError = String
type WriteError = String

class ConnectionReader a where
  readData :: a -> Int -> IO (Either ReadError ByteString)
  closeReader :: a -> IO ()
  openReader :: a -> IO ()

class ConnectionWriter a where
  writeData :: a -> ByteString -> IO (Either WriteError ())
  closeWriter :: a -> IO ()
  openWriter :: a -> IO ()

class (ConnectionReader a, ConnectionWriter a) => Connection a where
  closeConnection :: a -> IO ()
  openConnection :: a -> IO ()

