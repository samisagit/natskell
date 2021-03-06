module Types.Msg where

import           Data.ByteString

data Msg = Msg
  { subject   :: ByteString,
    sid       :: Int,
    replyTo   :: Maybe ByteString,
    byteCount :: Int,
    payload   :: Maybe ByteString
  } deriving (Eq, Show)
