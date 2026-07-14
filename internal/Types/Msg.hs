module Types.Msg
  ( Subject
  , SID
  , Payload
  , Headers
  , Msg (..)
  , messageContentSize
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

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

-- | Bytes retained for a NATS message body: encoded headers plus payload.
messageContentSize :: Maybe Headers -> Maybe Payload -> Int
messageContentSize maybeHeaders maybePayload =
  fromInteger (min (toInteger (maxBound :: Int)) totalBytes)
  where
    totalBytes = headerBytes + maybe 0 (toInteger . BS.length) maybePayload
    headerBytes :: Integer
    headerBytes =
      case maybeHeaders of
        Nothing -> 0
        Just headerPairs ->
          12 + sum (map headerPairSize headerPairs)
    headerPairSize (key, value) =
      toInteger (BS.length key) + toInteger (BS.length value) + 3
