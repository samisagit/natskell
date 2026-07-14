{-# LANGUAGE OverloadedStrings #-}

module JetStream.Protocol.Request
  ( requestMsg
  , requestJSON
  , requestJSONWithHeaders
  , requestUnit
  , decodeJetStreamResponse
  ) where

import qualified Client.API                 as Nats
import           Control.Monad              (void)
import           Data.Aeson
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Types           (Parser, parseEither)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           JetStream.Error
import           JetStream.Options
    ( JetStreamContext (..)
    , requestTimeoutMicros
    )
import           JetStream.Protocol.Headers (statusError)
import           JetStream.Types
    ( Headers
    , JetStreamRequestOption
    , Payload
    , Subject
    )

requestMsg :: JetStreamContext -> Subject -> Payload -> Headers -> [JetStreamRequestOption] -> IO (Either JetStreamError Nats.MsgView)
requestMsg ctx subject payload headers options = do
  result <- Nats.request (contextClient ctx) subject payload requestOptions
  pure $ case result of
    Left Nats.NatsRequestTimedOut -> Left JetStreamTimeout
    Left err                      -> Left (JetStreamNatsError err)
    Right msg                     -> Right msg
  where
    timeoutSeconds =
      fromRational (toRational (requestTimeoutMicros ctx options) / 1000000)
    requestOptions =
      Nats.withRequestTimeout timeoutSeconds
        : [Nats.withRequestHeaders headers | not (null headers)]

requestJSON :: FromJSON a => JetStreamContext -> Subject -> Maybe Value -> [JetStreamRequestOption] -> IO (Either JetStreamError a)
requestJSON ctx subject body =
  requestJSONWithHeaders ctx subject body []

requestJSONWithHeaders :: FromJSON a => JetStreamContext -> Subject -> Maybe Value -> Headers -> [JetStreamRequestOption] -> IO (Either JetStreamError a)
requestJSONWithHeaders ctx subject body headers options = do
  msgResult <- requestMsg ctx subject (maybe BS.empty strictEncode body) headers options
  pure $ do
    msg <- msgResult
    case statusError msg of
      Just err ->
        Left err
      Nothing ->
        decodeJetStreamResponse (Nats.payload msg)

requestUnit :: JetStreamContext -> Subject -> Maybe Value -> [JetStreamRequestOption] -> IO (Either JetStreamError ())
requestUnit ctx subject body options = do
  response <- requestJSON ctx subject body options
  pure (void (response :: Either JetStreamError SuccessResponse))

decodeJetStreamResponse :: FromJSON a => BS.ByteString -> Either JetStreamError a
decodeJetStreamResponse payload =
  case Aeson.eitherDecode (LBS.fromStrict payload) :: Either String Value of
    Left err ->
      Left (JetStreamDecodeError err)
    Right value ->
      case parseEither parseApiResult value of
        Left err ->
          Left (JetStreamDecodeError err)
        Right result ->
          result

parseApiResult :: FromJSON a => Value -> Parser (Either JetStreamError a)
parseApiResult value =
  case value of
    Object obj -> do
      apiError <- obj .:? "error"
      case apiError of
        Just err ->
          pure (Left (JetStreamApiFailure err))
        Nothing ->
          Right <$> parseJSON value
    _ ->
      Right <$> parseJSON value

strictEncode :: Value -> BS.ByteString
strictEncode =
  LBS.toStrict . Aeson.encode

newtype SuccessResponse = SuccessResponse Bool
  deriving (Eq, Show)

instance FromJSON SuccessResponse where
  parseJSON = withObject "SuccessResponse" $ \obj ->
    SuccessResponse <$> obj .: "success"
