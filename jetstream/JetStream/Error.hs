{-# LANGUAGE OverloadedStrings #-}

module JetStream.Error
  ( JetStreamApiError (..)
  , JetStreamError (..)
  ) where

import           Data.Aeson
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as E

data JetStreamApiError = JetStreamApiError
                           { apiErrorCode        :: Int
                           , apiErrorCodeDetail  :: Int
                           , apiErrorDescription :: BS.ByteString
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
      <$> obj .: "code"
      <*> obj .:? "err_code" .!= 0
      <*> fmap E.encodeUtf8 (obj .:? "description" .!= "")

instance ToJSON JetStreamApiError where
  toJSON err =
    object
      [ "code" .= apiErrorCode err
      , "err_code" .= apiErrorCodeDetail err
      , "description" .= E.decodeUtf8 (apiErrorDescription err)
      ]
