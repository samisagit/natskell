module Sid (nextSID) where

import qualified Data.ByteString    as BS
import           Data.Hashable      (hash)
import qualified Data.Text          as Text
import           Data.Text.Encoding (encodeUtf8)
import           System.Random
import           Types

nextSID :: StdGen -> (SID, StdGen)
nextSID std = (hash' bs, gen)
  where (bs, gen) = genByteString 16 std
        hash' = packStr' . show . abs . hash

packStr' :: String -> BS.ByteString
packStr' = encodeUtf8 . Text.pack

