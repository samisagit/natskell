{-# LANGUAGE OverloadedStrings #-}

module Types.Pub where

import           Data.ByteString           hiding (foldr, map)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           Data.Maybe
import           Prelude                   hiding (concat, length, null)
import           Transformers.Transformers (Transformer (..))
import           Types.Msg                 (Headers, Payload, Subject)
import           Validators.Validators

data Pub = Pub
             { subject :: Subject
             , replyTo :: Maybe Subject
             , headers :: Maybe Headers
             , payload :: Maybe Payload
             }
  deriving (Eq, Show)

instance Transformer Pub where
  transform pubMsg =
    case headers pubMsg of
      Nothing ->
        let payload' = payloadChunk (payload pubMsg)
            payloadLen = length payload'
            control = concat
              (["PUB ", subject pubMsg, " "] ++ replyChunks (replyTo pubMsg)
                ++ [packInt payloadLen, "\r\n"])
        in LBS.fromChunks [control, payload', "\r\n"]
      Just headerList ->
        let headers' = headerString headerList
            headerLength = length headers'
            payload' = payloadChunk (payload pubMsg)
            totalLen = headerLength + length payload'
            control = concat
              (["HPUB ", subject pubMsg, " "] ++ replyChunks (replyTo pubMsg)
                ++ [packInt headerLength, " ", packInt totalLen, "\r\n"])
        in LBS.fromChunks [control, headers', payload', "\r\n"]

instance Validator Pub where
  validate p = do
    validateSubject p
    validateReplyTo p
    validatePayload p
    validateHeaders p

validateSubject :: Pub -> Either ByteString ()
validateSubject p
  | subject p == "" = Left "explicit empty subject"
  | otherwise = Right ()

validateReplyTo :: Pub -> Either ByteString ()
validateReplyTo p
  | replyTo p == Just "" = Left "explicit empty replyTo"
  | otherwise = Right ()

validatePayload :: Pub -> Either ByteString ()
validatePayload p
  | payload p == Just "" = Left "explicit empty payload"
  | otherwise = Right ()

validateHeaders :: Pub -> Either ByteString ()
validateHeaders p
  | isNothing (headers p) = Right ()
  | headers p == Just [] = Left "explicit empty headers"
  | Prelude.any (\(k, _) -> k == "") (fromJust (headers p)) = Left "explicit empty header key"
  | Prelude.any (\(_, v) -> v == "") (fromJust (headers p)) = Left "explicit empty header value"
  | otherwise = Right ()

headerString :: Headers -> ByteString
headerString hs =
  concat ("NATS/1.0\r\n" : foldr appendHeader ["\r\n"] hs)
  where
    appendHeader (key, value) acc = key : ":" : value : "\r\n" : acc

payloadChunk :: Maybe Payload -> Payload
payloadChunk = fromMaybe empty

replyChunks :: Maybe Subject -> [ByteString]
replyChunks = maybe [] (\reply -> [reply, " "])

packInt :: Int -> ByteString
packInt = BC.pack . show
