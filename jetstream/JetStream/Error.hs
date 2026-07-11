{-# LANGUAGE OverloadedStrings #-}

module JetStream.Error
  ( JetStreamApiError (..)
  , JetStreamError (..)
  ) where

import           Data.Aeson
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as E

data JetStreamApiError = JetStreamApiError
                           { apiErrorCode        :: Maybe Int
                           , apiErrorCodeDetail  :: Maybe Int
                           , apiErrorDescription :: Maybe BS.ByteString
                           }
  deriving (Eq, Show)

data JetStreamError = JetStreamApiFailure JetStreamApiError
                    | JetStreamStatusError Int (Maybe BS.ByteString)
                    | JetStreamDecodeError String
                    | JetStreamTimeout
                    | JetStreamNoReply
  deriving (Eq, Show)

instance FromJSON JetStreamApiError where
  parseJSON = withObject "JetStreamApiError" $ \obj ->
    JetStreamApiError
      <$> obj .:? "code"
      <*> obj .:? "err_code"
      <*> fmap (fmap E.encodeUtf8) (obj .:? "description")

instance ToJSON JetStreamApiError where
  toJSON err =
    object
      ( maybe [] (\code -> ["code" .= code]) (apiErrorCode err)
          ++ maybe [] (\code -> ["err_code" .= code]) (apiErrorCodeDetail err)
          ++ maybe [] (\desc -> ["description" .= E.decodeUtf8 desc]) (apiErrorDescription err)
      )
