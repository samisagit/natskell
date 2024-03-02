module Types.Msg where

import           Data.ByteString

data Msg = Msg
  { subject :: ByteString,
    sid     :: ByteString,
    replyTo :: Maybe ByteString,
    payload :: Maybe ByteString,
    headers :: Maybe [(ByteString, ByteString)]
  } deriving (Eq, Show)
