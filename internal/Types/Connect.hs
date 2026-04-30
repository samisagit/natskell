{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Connect
  ( Connect (..)
  , validateAuthToken
  , validateUser
  , validatePass
  , validateName
  , validateLang
  , validateVersion
  , validateProtocol
  , validateSig
  , validateJwt
  , validateNKey
  ) where

import           Data.Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text.Encoding        as E
import           GHC.Generics              (Generic)
import           Transformers.Transformers (Transformer (..))
import           Validators.Validators

data Connect = Connect
                 { verbose       :: Bool
                 , pedantic      :: Bool
                 , tls_required  :: Bool
                 , auth_token    :: Maybe BS.ByteString
                 , user          :: Maybe BS.ByteString
                 , pass          :: Maybe BS.ByteString
                 , name          :: Maybe BS.ByteString
                 , lang          :: BS.ByteString
                 , version       :: BS.ByteString
                 , protocol      :: Maybe Int
                 , echo          :: Maybe Bool
                 , sig           :: Maybe BS.ByteString
                 , jwt           :: Maybe BS.ByteString
                 , nkey          :: Maybe BS.ByteString
                 , no_responders :: Maybe Bool
                 , headers       :: Maybe Bool
                 }
  deriving (Eq, Show)

instance ToJSON Connect where
  toJSON = toJSON . toConnectJSON

instance Transformer Connect where
  transform c =
    let encoded = encode c
    in LBS.fromChunks ["CONNECT ", LBS.toStrict encoded, "\r\n"]

newtype Utf8ByteString = Utf8ByteString { unUtf8ByteString :: BS.ByteString }
  deriving (Eq, Show)

instance ToJSON Utf8ByteString where
  toJSON = toJSON . E.decodeUtf8 . unUtf8ByteString

data ConnectJSON = ConnectJSON
                     { connectJSON_verbose       :: Bool
                     , connectJSON_pedantic      :: Bool
                     , connectJSON_tls_required  :: Bool
                     , connectJSON_auth_token    :: Maybe Utf8ByteString
                     , connectJSON_user          :: Maybe Utf8ByteString
                     , connectJSON_pass          :: Maybe Utf8ByteString
                     , connectJSON_name          :: Maybe Utf8ByteString
                     , connectJSON_lang          :: Utf8ByteString
                     , connectJSON_version       :: Utf8ByteString
                     , connectJSON_protocol      :: Maybe Int
                     , connectJSON_echo          :: Maybe Bool
                     , connectJSON_sig           :: Maybe Utf8ByteString
                     , connectJSON_jwt           :: Maybe Utf8ByteString
                     , connectJSON_nkey          :: Maybe Utf8ByteString
                     , connectJSON_no_responders :: Maybe Bool
                     , connectJSON_headers       :: Maybe Bool
                     }
  deriving (Eq, Generic, Show)

instance ToJSON ConnectJSON where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop connectJSONPrefixLength
    , omitNothingFields = True
    }

connectJSONPrefixLength :: Int
connectJSONPrefixLength = 12

toConnectJSON :: Connect -> ConnectJSON
toConnectJSON c =
  ConnectJSON
    { connectJSON_verbose = verbose c
    , connectJSON_pedantic = pedantic c
    , connectJSON_tls_required = tls_required c
    , connectJSON_auth_token = Utf8ByteString <$> auth_token c
    , connectJSON_user = Utf8ByteString <$> user c
    , connectJSON_pass = Utf8ByteString <$> pass c
    , connectJSON_name = Utf8ByteString <$> name c
    , connectJSON_lang = Utf8ByteString (lang c)
    , connectJSON_version = Utf8ByteString (version c)
    , connectJSON_protocol = protocol c
    , connectJSON_echo = echo c
    , connectJSON_sig = Utf8ByteString <$> sig c
    , connectJSON_jwt = Utf8ByteString <$> jwt c
    , connectJSON_nkey = Utf8ByteString <$> nkey c
    , connectJSON_no_responders = no_responders c
    , connectJSON_headers = headers c
    }

instance Validator Connect where
  validate c = do
    validateAuthToken c
    validateUser c
    validatePass c
    validateName c
    validateLang c
    validateVersion c
    validateProtocol c
    validateSig c
    validateJwt c
    validateNKey c

validateAuthToken :: Connect -> Either BS.ByteString ()
validateAuthToken c
  | auth_token c == Just "" = Left "explicit empty auth token"
  | otherwise = Right ()

validateUser :: Connect -> Either BS.ByteString ()
validateUser c
  | user c == Just "" = Left "explicit empty user"
  | otherwise = Right ()

validatePass :: Connect -> Either BS.ByteString ()
validatePass c
  | pass c == Just "" = Left "explicit empty pass"
  | otherwise = Right ()

validateName :: Connect -> Either BS.ByteString ()
validateName c
  | name c == Just "" = Left "explicit empty name"
  | otherwise = Right ()

validateLang :: Connect -> Either BS.ByteString ()
validateLang c
  | lang c ==  "" = Left "explicit empty lang"
  | otherwise = Right ()

validateVersion :: Connect -> Either BS.ByteString ()
validateVersion c
  | version c == "" = Left "explicit empty version"
  | otherwise = Right ()

validateProtocol :: Connect -> Either BS.ByteString ()
validateProtocol c
  | protocol c `notElem` [Nothing, Just 0, Just 1] = Left "invalid protocol"
  | otherwise = Right ()

validateSig :: Connect -> Either BS.ByteString ()
validateSig c
  | sig c == Just "" = Left "explicit empty sig"
  | otherwise = Right ()

validateJwt :: Connect -> Either BS.ByteString ()
validateJwt c
  | jwt c == Just "" = Left "explicit empty jwt"
  | otherwise = Right ()

validateNKey :: Connect -> Either BS.ByteString ()
validateNKey c
  | nkey c == Just "" = Left "explicit empty nkey"
  | otherwise = Right ()
