{-# LANGUAGE OverloadedStrings #-}

module JetStream.Protocol.Request
  ( requestMsg
  , requestJSON
  , requestJSONWithHeaders
  , requestUnit
  , decodeJetStreamResponse
  ) where

import qualified Client.API                 as Nats
import           Control.Concurrent.STM
import           Control.Monad              (void)
import           Data.Aeson
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Types           (Parser, parseEither)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           JetStream.Error
import           JetStream.Options          (JetStreamContext (..))
import           JetStream.Protocol.Headers (statusError)
import           JetStream.Types            (Headers, Payload, Subject)
import           System.Timeout             (timeout)

requestMsg :: JetStreamContext -> Subject -> Maybe Payload -> Headers -> IO (Either JetStreamError Nats.MsgView)
requestMsg ctx subject payload headers = do
  replyBox <- newEmptyTMVarIO
  let publishOptions =
        maybe [] (\body -> [Nats.withPayload body]) payload
          ++ [Nats.withReplyCallback (atomically . putTMVar replyBox)]
          ++ [Nats.withHeaders headers | not (null headers)]
  Nats.publish (contextClient ctx) subject publishOptions
  result <- timeout (contextRequestTimeoutMicros ctx) (atomically (readTMVar replyBox))
  case result of
    Nothing ->
      pure (Left JetStreamTimeout)
    Just Nothing ->
      pure (Left JetStreamNoReply)
    Just (Just msg) ->
      pure (Right msg)

requestJSON :: FromJSON a => JetStreamContext -> Subject -> Maybe Value -> IO (Either JetStreamError a)
requestJSON ctx subject body =
  requestJSONWithHeaders ctx subject body []

requestJSONWithHeaders :: FromJSON a => JetStreamContext -> Subject -> Maybe Value -> Headers -> IO (Either JetStreamError a)
requestJSONWithHeaders ctx subject body headers = do
  msgResult <- requestMsg ctx subject (strictEncode <$> body) headers
  pure $ do
    msg <- msgResult
    case statusError msg of
      Just err ->
        Left err
      Nothing ->
        case Nats.payload msg of
          Nothing ->
            Left JetStreamNoReply
          Just payload ->
            decodeJetStreamResponse payload

requestUnit :: JetStreamContext -> Subject -> Maybe Value -> IO (Either JetStreamError ())
requestUnit ctx subject body = do
  response <- requestJSON ctx subject body
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
