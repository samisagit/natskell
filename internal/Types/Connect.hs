{-# LANGUAGE OverloadedStrings #-}

module Types.Connect
  ( module Types.Connect.Types
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
  , textToByteString
  , byteStringToText
  ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString       as BS
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E
import           Types.Connect.Types
import           Validators.Validators

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

textToByteString :: MonadPlus m =>  T.Text -> m BS.ByteString
textToByteString x = pure $ E.encodeUtf8 x

byteStringToText :: BS.ByteString -> T.Text
byteStringToText = E.decodeUtf8
