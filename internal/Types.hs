module Types where

import qualified Data.ByteString as BS

type Subject = BS.ByteString
type SID = BS.ByteString
type Payload = BS.ByteString
type Headers = [(BS.ByteString, BS.ByteString)]
