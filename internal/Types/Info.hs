module Types.Info
  ( module Types.Info.Types
  , textToByteString
  , byteStringToText
  ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import           Types.Info.Types

instance FromJSON Info
instance ToJSON Info

instance ToJSON BS.ByteString where
  toJSON = toJSON . byteStringToText

instance FromJSON BS.ByteString where
  parseJSON (String x) = textToByteString x
  parseJSON _          = mzero

textToByteString :: MonadPlus m =>  T.Text -> m BS.ByteString
textToByteString x = pure $ E.encodeUtf8 x

byteStringToText :: BS.ByteString -> T.Text
byteStringToText = E.decodeUtf8
