{-# LANGUAGE DeriveGeneric #-}

module Types.Info
  ( Info (..)
  ) where

import           Data.Aeson
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as E
import           GHC.Generics       (Generic)

data Info = Info
              { server_id     :: BS.ByteString
              , version       :: BS.ByteString
              , go            :: BS.ByteString
              , host          :: BS.ByteString
              , port          :: Int
              , max_payload   :: Int
              , proto         :: Int
              , client_id     :: Maybe Int
              , nonce         :: Maybe BS.ByteString
              , auth_required :: Maybe Bool
              , tls_required  :: Maybe Bool
              , connect_urls  :: Maybe [BS.ByteString]
              , ldm           :: Maybe Bool
              , headers       :: Maybe Bool
              }
  deriving (Eq, Show)

instance FromJSON Info where
  parseJSON = fmap fromInfoJSON . genericParseJSON defaultOptions
    { fieldLabelModifier = drop infoJSONPrefixLength
    }

infoJSONPrefixLength :: Int
infoJSONPrefixLength = 9

newtype Utf8ByteString = Utf8ByteString { unUtf8ByteString :: BS.ByteString }
  deriving (Eq, Show)

instance FromJSON Utf8ByteString where
  parseJSON = fmap (Utf8ByteString . E.encodeUtf8) . parseJSON

data InfoJSON = InfoJSON
                  { infoJSON_server_id     :: Utf8ByteString
                  , infoJSON_version       :: Utf8ByteString
                  , infoJSON_go            :: Utf8ByteString
                  , infoJSON_host          :: Utf8ByteString
                  , infoJSON_port          :: Int
                  , infoJSON_max_payload   :: Int
                  , infoJSON_proto         :: Int
                  , infoJSON_client_id     :: Maybe Int
                  , infoJSON_nonce         :: Maybe Utf8ByteString
                  , infoJSON_auth_required :: Maybe Bool
                  , infoJSON_tls_required  :: Maybe Bool
                  , infoJSON_connect_urls  :: Maybe [Utf8ByteString]
                  , infoJSON_ldm           :: Maybe Bool
                  , infoJSON_headers       :: Maybe Bool
                  }
  deriving (Eq, Generic, Show)

fromInfoJSON :: InfoJSON -> Info
fromInfoJSON infoJson =
  Info
    { server_id = unUtf8ByteString (infoJSON_server_id infoJson)
    , version = unUtf8ByteString (infoJSON_version infoJson)
    , go = unUtf8ByteString (infoJSON_go infoJson)
    , host = unUtf8ByteString (infoJSON_host infoJson)
    , port = infoJSON_port infoJson
    , max_payload = infoJSON_max_payload infoJson
    , proto = infoJSON_proto infoJson
    , client_id = infoJSON_client_id infoJson
    , nonce = unUtf8ByteString <$> infoJSON_nonce infoJson
    , auth_required = infoJSON_auth_required infoJson
    , tls_required = infoJSON_tls_required infoJson
    , connect_urls = fmap (map unUtf8ByteString) (infoJSON_connect_urls infoJson)
    , ldm = infoJSON_ldm infoJson
    , headers = infoJSON_headers infoJson
    }
