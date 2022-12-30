module Types.Msg where

import           Data.ByteString

data Msg = Msg
  { subject   :: ByteString,
    sid       :: ByteString,
    replyTo   :: Maybe ByteString,
    byteCount :: Int, -- TODO: not sure we need to store this
    payload   :: Maybe ByteString,
    headers   :: Maybe [(ByteString, ByteString)]
  } deriving (Eq, Show)
