{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Connect where

import           Data.Aeson
import qualified Data.ByteString as BS
import           Data.Text       (Text)
import           GHC.Generics
import           Parser

data Data = Data
  {
    verbose      :: Bool,
    pedantic     :: Bool,
    tls_required :: Bool,
    auth_token   :: Maybe Text,
    user         :: Maybe Text,
    pass         :: Maybe Text,
    name         :: Maybe Text,
    lang         :: Text,
    version      :: Int,
    protocol     :: Maybe Int,
    echo         :: Maybe Bool,
    sig          :: Maybe Text,
    jwt          :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Data where
  parseJSON = genericParseJSON defaultOptions
    {omitNothingFields = True}

instance ToJSON Data where
  toJSON = genericToJSON defaultOptions
    {omitNothingFields = True}

transform :: Data -> BS.ByteString
transform d = do
  let f = encode d
  BS.append ("CONNECT " :: BS.ByteString) (BS.toStrict f)
