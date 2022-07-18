{-# LANGUAGE OverloadedStrings #-}

module Transformers where

import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text
import           Data.Text.Encoding   (encodeUtf8)
import           Text.Printf
import           Types.Connect
import           Types.Ping
import           Types.Pong
import qualified Types.Pub            as Pub
import qualified Types.Sub            as Sub
import qualified Types.Unsub          as Unsub

class Transformer a where
  transform :: a -> BS.ByteString

instance Transformer Ping where
  transform _ = "PING\r\n"

instance Transformer Pong where
  transform _ = "PING\r\n"

instance Transformer Connect where
  transform c = do
    let f = encode c
    BS.append ("CONNECT " :: BS.ByteString) (LBS.toStrict f)


instance Transformer Pub.Pub where
  transform d = foldr BS.append "" list
    where
      list = [
        "PUB",
        " ",
        Pub.subject d,
        " ",
        collapseNothing (Pub.replyTo d) " ",
        packStr' (printf "%v" (lengthNothing . Pub.payload $ d)),
        "\r\n",
        collapseNothing (Pub.payload d) "\r\n"
        ]

instance Transformer Sub.Sub where
  transform d = do
    case Sub.queueGroup d of
       Just a  -> packStr' (printf "SUB %s %s %s" subj a id)
       Nothing -> packStr' (printf "SUB %s %s" subj id)
    where
      subj = Sub.subject d
      qg = Sub.queueGroup d
      id = Sub.sid d

instance Transformer Unsub.Unsub where
  transform d = do
    case mm of
       Just a  -> packStr' (printf "UNSUB %s %u" id a)
       Nothing -> packStr' (printf "UNSUB %s" id)
    where
      id = Unsub.sid d
      mm = Unsub.maxMsg d

collapseNothing :: Maybe BS.ByteString -> BS.ByteString -> BS.ByteString
collapseNothing mbs suffix = case mbs of
  Just a  -> BS.append a suffix
  Nothing -> ""

lengthNothing :: Maybe BS.ByteString -> Int
lengthNothing = maybe 0 BS.length

packStr' :: String -> BS.ByteString
packStr' = encodeUtf8 . Text.pack

