{-# LANGUAGE DeriveGeneric #-}

module Types.Info where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

data Info = Info
  { server_id     :: Text,
    version       :: Text,
    go            :: Text,
    host          :: Text,
    port          :: Int,
    max_payload   :: Int,
    proto         :: Int,
    client_id     :: Maybe Int,
    auth_required :: Maybe Bool,
    tls_required  :: Maybe Bool,
    connect_urls  :: Maybe [String],
    ldm           :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON Info
instance ToJSON Info
