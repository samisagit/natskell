module Sid (sidGen, nextSID) where

import qualified Data.ByteString    as BS
import           Data.Hashable      (hash)
import qualified Data.Text          as Text
import           Data.Text.Encoding (encodeUtf8)
import           Data.UUID
import           Data.UUID.V4
import           Types
import System.Random

sidGen :: IO SID
sidGen = do
  sid <- fmap (BS.toStrict . toByteString) nextRandom
  return . packStr' . show . abs $ hash sid

nextSID :: StdGen -> (SID, StdGen)
nextSID std = (hash' bs, gen)
  where (bs, gen) = genByteString 16 std
        hash' = packStr' . show . abs . hash

packStr' :: String -> BS.ByteString
packStr' = encodeUtf8 . Text.pack

