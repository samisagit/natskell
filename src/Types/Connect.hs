{-# LANGUAGE DeriveGeneric #-}

module Types.Connect where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

data Connect = Connect
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
instance ToJSON Connect where
  toJSON = genericToJSON defaultOptions
    {omitNothingFields = True}

