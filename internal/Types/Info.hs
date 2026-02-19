{-# LANGUAGE DeriveGeneric #-}

module Types.Info where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import           GHC.Generics

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
  deriving (Eq, Generic, Show)

instance FromJSON Info
instance ToJSON Info

instance ToJSON BS.ByteString where
  toJSON = toJSON . byteStringToText

instance FromJSON BS.ByteString where
  parseJSON (String x) = textToByteString x
  parseJSON _          = mzero

textToByteString :: MonadPlus m =>  T.Text -> m BS.ByteString
textToByteString x = pure $ E.encodeUtf8 x

byteStringToText :: BS.ByteString -> T.Text
byteStringToText = E.decodeUtf8
