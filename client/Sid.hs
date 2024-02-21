module Sid (sidGen) where

import qualified Data.ByteString as BS
import qualified Data.Text            as Text
import           Data.Text.Encoding   (encodeUtf8)
import           Data.UUID.V4
import           Data.UUID
import Data.Hashable (hash)
import Types


sidGen :: IO SID
sidGen = do
  sid <- fmap (BS.toStrict . toByteString) nextRandom
  return . packStr' . show . abs $ hash sid

packStr' :: String -> BS.ByteString
packStr' = encodeUtf8 . Text.pack

