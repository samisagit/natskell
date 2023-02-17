module Types.Err where

import           Data.ByteString

data Err = Err {
  reason :: ByteString,
  fatal  :: Bool
} deriving (Show, Eq)

