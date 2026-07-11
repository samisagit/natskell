{-# LANGUAGE OverloadedStrings #-}

module JetStream.Protocol.Headers
  ( statusHeader
  , descriptionHeader
  , lookupHeader
  , statusCode
  , statusDescription
  , statusError
  ) where

import qualified API                   as Nats
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe            (fromMaybe, listToMaybe)
import           JetStream.Error       (JetStreamError (JetStreamStatusError))

statusHeader :: BS.ByteString
statusHeader = "Status"

descriptionHeader :: BS.ByteString
descriptionHeader = "Description"

lookupHeader :: BS.ByteString -> Nats.MsgView -> Maybe BS.ByteString
lookupHeader name msg =
  listToMaybe
    [ value
    | (key, value) <- fromMaybe [] (Nats.headers msg)
    , key == name
    ]

statusCode :: Nats.MsgView -> Maybe Int
statusCode msg = do
  value <- lookupHeader statusHeader msg
  case BC.readInt value of
    Just (code, rest)
      | BS.null rest ->
          Just code
    _ ->
      Nothing

statusDescription :: Nats.MsgView -> Maybe BS.ByteString
statusDescription =
  lookupHeader descriptionHeader

statusError :: Nats.MsgView -> Maybe JetStreamError
statusError msg =
  fmap (\code -> JetStreamStatusError code (statusDescription msg)) (statusCode msg)
