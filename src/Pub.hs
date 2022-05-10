{-# LANGUAGE OverloadedStrings #-}

module Pub where

import qualified Data.ByteString    as BS
import qualified Data.Maybe
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Text.Printf

data Data = Data
  { subject :: BS.ByteString,
    replyTo :: Maybe BS.ByteString,
    payload :: Maybe BS.ByteString
  }

transform :: Data -> BS.ByteString
transform d = foldr BS.append "" list
  where
    list = [
      "PUB",
      " ",
      subject d,
      " ",
      collapseNothing (replyTo d) " ",
      packStr' (printf "%v" (lengthNothing . payload $ d)),
      "\r\n",
      collapseNothing (payload d) "\r\n"
      ]

collapseNothing :: Maybe BS.ByteString -> BS.ByteString -> BS.ByteString
collapseNothing mbs sep = case mbs of
  Just a  -> BS.append a sep
  Nothing -> ""

lengthNothing :: Maybe BS.ByteString -> Int
lengthNothing = maybe 0 BS.length

packStr' :: String -> BS.ByteString
packStr' = encodeUtf8 . T.pack
