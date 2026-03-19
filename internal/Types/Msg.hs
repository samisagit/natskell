module Types.Msg
  ( Subject
  , SID
  , Payload
  , Headers
  , Msg (..)
  ) where

import           Data.ByteString (ByteString)

type Subject = ByteString
type SID = ByteString
type Payload = ByteString
type Headers = [(ByteString, ByteString)]

data Msg = Msg
             { subject :: Subject
             , sid     :: SID
             , replyTo :: Maybe Subject
             , payload :: Maybe Payload
             , headers :: Maybe Headers
             }
  deriving (Eq, Show)
