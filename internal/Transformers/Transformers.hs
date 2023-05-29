{-# LANGUAGE OverloadedStrings #-}

module Transformers.Transformers where

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
  transform _ = "PONG\r\n"

instance Transformer Connect where
  transform c = do
    let f = encode c
    foldr BS.append "" ["CONNECT ", LBS.toStrict f, "\r\n"]

instance Transformer Pub.Pub where
  transform d = case Pub.headers d of
    Nothing -> foldr BS.append "" [
      "PUB",
      " ",
      Pub.subject d,
      " ",
      collapseNothing (Pub.replyTo d) " ",
      packStr' (printf "%v" (lengthNothing . Pub.payload $ d)),
      "\r\n",
      collapseNothing (Pub.payload d) "",
      "\r\n"
      ]
    Just hs -> foldr BS.append "" [
      "HPUB",
      " ",
      Pub.subject d,
      " ",
      collapseNothing (Pub.replyTo d) " ",
      packStr' (printf "%v" (BS.length . headerString $ hs)),
      " ",
      packStr' (printf "%v" ((lengthNothing . Pub.payload $ d) + (BS.length . headerString $ hs))),
      "\r\n",
      headerString hs,
      "\r\n",
      collapseNothing (Pub.payload d) "",
      "\r\n"
      ]

headerString :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
headerString = BS.concat . map (\(k, v) -> foldr BS.append "" [k, ":", v, "\r\n"])


instance Transformer Sub.Sub where
  transform d = do
    case qg of
       Just a  -> foldr BS.append "" ["SUB ", subj, " ", a, " ", id, "\r\n"]
       Nothing -> foldr BS.append "" ["SUB ", subj, " ", id, "\r\n"]
    where
      subj = Sub.subject d
      qg = Sub.queueGroup d
      id = Sub.sid d

instance Transformer Unsub.Unsub where
  transform d = do
    case mm of
       Just a  -> foldr BS.append "" ["UNSUB ", id, " ", packInt a, "\r\n"]
       Nothing -> foldr BS.append "" ["UNSUB ", id, "\r\n"]
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

packInt = packStr' . show
