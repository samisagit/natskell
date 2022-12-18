{-# LANGUAGE DeriveGeneric #-}

module Types.Connect where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import           GHC.Generics

data Connect = Connect
  {
    verbose      :: Bool,
    pedantic     :: Bool,
    tls_required :: Bool,
    auth_token   :: Maybe BS.ByteString,
    user         :: Maybe BS.ByteString,
    pass         :: Maybe BS.ByteString,
    name         :: Maybe BS.ByteString,
    lang         :: BS.ByteString,
    version      :: BS.ByteString,
    protocol     :: Maybe Int,
    echo         :: Maybe Bool,
    sig          :: Maybe BS.ByteString,
    jwt          :: Maybe BS.ByteString
  }
  deriving (Eq, Show, Generic)

instance ToJSON Connect where
  toJSON = genericToJSON defaultOptions
    {omitNothingFields = True}

instance ToJSON BS.ByteString where
  toJSON = toJSON . byteStringToText

instance FromJSON BS.ByteString where
  parseJSON (String x) = textToByteString x
  parseJSON _          = mzero

textToByteString :: MonadPlus m =>  T.Text -> m BS.ByteString
textToByteString x = pure $ E.encodeUtf8 x

byteStringToText :: BS.ByteString -> T.Text
byteStringToText = E.decodeUtf8
