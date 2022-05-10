{-# LANGUAGE OverloadedStrings #-}

module Unsub where

import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Text.Printf

data Data = Data
  { sid    :: String,
    maxMsg :: Maybe Int
  }

transform :: Data -> BS.ByteString
transform d = do
  case mm of
     Just a  -> packStr' (printf "UNSUB %s %u" id a)
     Nothing -> packStr' (printf "UNSUB %s" id)
  where
    id = sid d
    mm = maxMsg d

packStr' :: String -> BS.ByteString
packStr' = encodeUtf8 . T.pack
