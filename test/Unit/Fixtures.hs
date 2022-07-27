{-# LANGUAGE OverloadedStrings #-}

module Fixtures where

import qualified Data.ByteString    as BS
import           Data.Text          (Text, pack)
import           Data.Text.Encoding
import           Text.Printf

versionCases = ["0.0.0", "v1.0.1", "13.0.0+123"]
boolCases = [True, False]
userCases = ["samisagit", "sam@google.com"]
passCases = ["dsalkj09898(*)(UHJHI&*&*)(910"]
nameCases = ["natskell-client"]
sigCases = ["eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQSflKxwRJSMeKKF2QT4fwpMeJf36POk6yJVadQssw5c"]
jwtCases = ["eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"]
intCases = [1]
goVersionCases = ["1.17"]
hostCases = ["192.168.1.7"]
portCases = [4222]
serverIDCases = ["123"]
maxPayloadCases = [1024]
protocolCases = [1,2,3]
clientIDCases = [1, 100]
connectStringCases = [[], ["127.0.0.1"], ["127.0.0.1:4222", "0.0.0.0:4222"]]

maybeify :: [a] -> [Maybe a]
maybeify xs = Nothing : fmap Just xs

collapseMaybeStringField :: BS.ByteString -> Maybe Text -> BS.ByteString
collapseMaybeStringField f v  = case v of
  Nothing -> ""
  Just a  -> foldr BS.append "" ["\"", f, "\":", "\"", encodeUtf8 a, "\","]

collapseMaybeBoolField :: BS.ByteString -> Maybe Bool -> BS.ByteString
collapseMaybeBoolField f v  = case v of
  Nothing    -> ""
  Just True  -> foldr BS.append "" ["\"", f, "\":", "true,"]
  Just False -> foldr BS.append "" ["\"", f, "\":", "false,"]

collapseMaybeIntField :: BS.ByteString -> Maybe Int -> BS.ByteString
collapseMaybeIntField f v = case v of
  Nothing -> ""
  Just a  -> foldr BS.append "" ["\"", f, "\":", encodeUtf8 . pack . show $ a, ","]

collapseMaybeStringListField :: BS.ByteString -> Maybe [String] -> BS.ByteString
collapseMaybeStringListField f v  = case v of
  Nothing -> ""
  Just [] -> foldr BS.append "" ["\"", f, "\"", ":[],"]
  Just a  -> foldr BS.append "" ["\"", f, "\"", ":[", encodeUtf8 . pack $ formattedItems, "]", ","]
    where
      formattedItems = init $ concatMap (printf "\"%s\",") a :: String

