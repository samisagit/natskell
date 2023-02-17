{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Connect where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString       as BS
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E
import           GHC.Generics
import           Validators.Validators

data Connect = Connect
  {
    verbose       :: Bool,
    pedantic      :: Bool,
    tls_required  :: Bool,
    auth_token    :: Maybe BS.ByteString,
    user          :: Maybe BS.ByteString,
    pass          :: Maybe BS.ByteString,
    name          :: Maybe BS.ByteString,
    lang          :: BS.ByteString,
    version       :: BS.ByteString,
    protocol      :: Maybe Int,
    echo          :: Maybe Bool,
    sig           :: Maybe BS.ByteString,
    jwt           :: Maybe BS.ByteString,
    no_responders :: Maybe Bool
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

instance Validator Connect where
  validate c
    | auth_token c == Just "" = Just "explicit empty auth token"
    | user c == Just "" = Just "explicit empty user"
    | pass c == Just "" = Just "explicit empty pass"
    | name c == Just "" = Just "explicit empty name"
    | lang c ==  "" = Just "explicit empty lang"
    | version c == "" = Just "explicit empty version"
    | protocol c `notElem` [Nothing, Just 0, Just 1] = Just "invalid protocol"
    | sig c == Just "" = Just "explicit empty sig"
    | jwt c == Just "" = Just "explicit empty jwt"
    | otherwise = Nothing

textToByteString :: MonadPlus m =>  T.Text -> m BS.ByteString
textToByteString x = pure $ E.encodeUtf8 x

byteStringToText :: BS.ByteString -> T.Text
byteStringToText = E.decodeUtf8
