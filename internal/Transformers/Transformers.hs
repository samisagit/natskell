{-# LANGUAGE OverloadedStrings #-}

module Transformers.Transformers
  ( module Transformers.Types
  , headerString
  , payloadChunk
  , replyChunks
  , packStr'
  , packInt
  , transformersApi
  ) where

import           Data.Aeson
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import           Data.Maybe                   (fromMaybe)
import qualified Data.Text                    as Text
import           Data.Text.Encoding           (encodeUtf8)
import           Transformers.TransformersAPI (TransformersAPI (..))
import           Transformers.Types
import           Types.Connect
import           Types.Ping
import           Types.Pong
import qualified Types.Pub                    as Pub
import qualified Types.Sub                    as Sub
import qualified Types.Unsub                  as Unsub

instance Transformer Ping where
  transform _ = LBS.fromStrict "PING\r\n"

instance Transformer Pong where
  transform _ = LBS.fromStrict "PONG\r\n"

instance Transformer Connect where
  transform c = do
    let f = encode c
    LBS.fromChunks ["CONNECT ", LBS.toStrict f, "\r\n"]

instance Transformer Pub.Pub where
  transform d =
    case Pub.headers d of
      Nothing ->
        let payload = payloadChunk (Pub.payload d)
            payloadLen = BS.length payload
            control = BS.concat
              (["PUB ", Pub.subject d, " "] ++ replyChunks (Pub.replyTo d)
                ++ [packInt payloadLen, "\r\n"])
        in LBS.fromChunks [control, payload, "\r\n"]
      Just hs ->
        let headers = headerString hs
            headerLength = BS.length headers
            payload = payloadChunk (Pub.payload d)
            totalLen = headerLength + BS.length payload
            control = BS.concat
              (["HPUB ", Pub.subject d, " "] ++ replyChunks (Pub.replyTo d)
                ++ [packInt headerLength, " ", packInt totalLen, "\r\n"])
        in LBS.fromChunks [control, headers, payload, "\r\n"]

headerString :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
headerString hs =
  BS.concat ("NATS/1.0\r\n" : foldr appendHeader ["\r\n"] hs)
  where
    appendHeader (key, value) acc = key : ":" : value : "\r\n" : acc

instance Transformer Sub.Sub where
  transform d = do
    case qg of
       Just a  -> LBS.fromChunks ["SUB ", subj, " ", a, " ", id, "\r\n"]
       Nothing -> LBS.fromChunks ["SUB ", subj, " ", id, "\r\n"]
    where
      subj = Sub.subject d
      qg = Sub.queueGroup d
      id = Sub.sid d

instance Transformer Unsub.Unsub where
  transform d = do
    case mm of
       Just a  -> LBS.fromChunks ["UNSUB ", id, " ", packInt a, "\r\n"]
       Nothing -> LBS.fromChunks ["UNSUB ", id, "\r\n"]
    where
      id = Unsub.sid d
      mm = Unsub.maxMsg d

payloadChunk :: Maybe BS.ByteString -> BS.ByteString
payloadChunk = fromMaybe BS.empty

replyChunks :: Maybe BS.ByteString -> [BS.ByteString]
replyChunks = maybe [] (\reply -> [reply, " "])

packStr' :: String -> BS.ByteString
packStr' = encodeUtf8 . Text.pack

packInt = packStr' . show

transformersApi :: TransformersAPI
transformersApi = TransformersAPI
  { transformersTransform = transform
  }
