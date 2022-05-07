{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Info where

import           Data.Aeson
import qualified Data.ByteString as BS
import           Data.Text       (Text)
import           GHC.Generics
import           Parser

data Data = Data
  { server_id     :: Text,
    version       :: Text,
    go            :: Text,
    host          :: Text,
    port          :: Int,
    max_payload   :: Int,
    proto         :: Int,
    client_id     :: Maybe Int,
    auth_required :: Bool,
    tls_required  :: Bool,
    connect_urls  :: Maybe [String],
    ldm           :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON Data

instance ToJSON Data

parser :: Parser (Maybe Data)
parser = do
  string "INFO"
  ss
  rest <- asciis
  let packed = BS.pack rest
  return (decode (BS.fromStrict packed))

