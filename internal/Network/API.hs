module Network.API where

import           Data.ByteString

type ReadError = String
type WriteError = String

class ConnectionReader a where
  readData :: a -> Int -> IO (Either ReadError ByteString)
  closeReader :: a -> IO ()

class ConnectionWriter a where
  writeData :: a -> ByteString -> IO (Either WriteError ())
  closeWriter :: a -> IO ()

class (ConnectionReader a, ConnectionWriter a) => Connection a where
  closeConnection :: a -> IO ()

-- versioned insights?
-- schedule time error?
-- add closure context on spans

