{-# LANGUAGE OverloadedStrings #-}

module Sub where

import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Text.Printf

data Data = Data
  { subject    :: String,
    queueGroup :: Maybe String,
    sid        :: String
  }

transform :: Data -> BS.ByteString
transform d = do
  case queueGroup d of
     Just a  -> packStr' (printf "SUB %s %s %s" subj a id)
     Nothing -> packStr' (printf "SUB %s %s" subj id)
  where
    subj = subject d
    qg = queueGroup d
    id = sid d

packStr' :: String -> BS.ByteString
packStr' = encodeUtf8 . T.pack
