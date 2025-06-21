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
                 , no_responders :: Maybe Bool
                 , headers       :: Maybe Bool
                 }
  deriving (Eq, Generic, Show)

instance ToJSON Connect where
  toJSON = genericToJSON defaultOptions
    {omitNothingFields = True}

instance ToJSON BS.ByteString where
  toJSON = toJSON . byteStringToText

instance FromJSON BS.ByteString where
  parseJSON (String x) = textToByteString x
  parseJSON _          = mzero

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

textToByteString :: MonadPlus m =>  T.Text -> m BS.ByteString
textToByteString x = pure $ E.encodeUtf8 x

byteStringToText :: BS.ByteString -> T.Text
byteStringToText = E.decodeUtf8
