module Types.Pub where

import           Data.ByteString

data Pub = Pub
  { subject :: ByteString,
    replyTo :: Maybe ByteString,
    payload :: Maybe ByteString
  }
  deriving (Eq, Show)

