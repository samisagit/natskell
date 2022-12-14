{-# LANGUAGE OverloadedStrings #-}

module InfoSpec (spec) where

import           Control.Monad
import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Fixtures
import           Lib.Parser
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Types.Info

spec :: Spec
spec = do
  manual
  applicative
cases :: [(BS.ByteString, Info, String)]
cases = [
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3, \"client_id\": 1, \"auth_required\": true, \"tls_required\": true, \"connect_urls\": [\"https://127.0.0.1:4222\"], \"ldm\": true}\r\n",
    Info "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 (Just 1) (Just True) (Just True) (Just ["https://127.0.0.1:4222"]) (Just True),
    "fully populated"
  ),
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3}\r\n",
    Info "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 Nothing Nothing Nothing Nothing Nothing,
    "minimal"
  ),
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3, \"client_id\": 1, \"auth_required\": true, \"tls_required\": true, \"connect_urls\": [\"https://127.0.0.1:4222\", \"https://192.168.9.7:4222\"], \"ldm\": true}\r\n",
    Info "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 (Just 1) (Just True) (Just True) (Just ["https://127.0.0.1:4222", "https://192.168.9.7:4222"]) (Just True),
    "multi connect urls"
  )
  ]

manual = parallel $ do
  describe "specific parser" $ do
    forM_ cases $ \(input, expected, name) ->
      it (printf "parses %s case successfully" name) $ do
        let output = runParser infoParser input
        let result = fmap fst output
        let rest = fmap snd output
        case result of
          Just (ParsedInfo a) -> a `shouldBe` expected
          Nothing             -> error "parser did not return INFO type"
        case rest of
          Just "" -> return ()
          _       -> error "parser did not consume all tokens"
  describe "generic parser" $ do
    forM_ cases $ \(input, expected, name) ->
      it (printf "parses %s case successfully" name) $ do
        let output = genericParse input
        output `shouldBe` Just (ParsedInfo expected)

applicative = parallel $ do
  describe "generated" $ do
    let infos = Info
          <$> serverIDCases
          <*> versionCases
          <*> goVersionCases
          <*> hostCases
          <*> portCases
          <*> maxPayloadCases
          <*> protocolCases
          <*> maybeify clientIDCases
          <*> maybeify boolCases
          <*> maybeify boolCases
          <*> maybeify connectStringCases
          <*> maybeify boolCases
    let proto = map buildProtoInput infos
    let pairs = zip proto infos
    forM_ pairs $ \(input, want) -> do
      it (printf "correctly parses %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Just (ParsedInfo want)

buildProtoInput :: Info -> BS.ByteString
buildProtoInput m = foldr BS.append "" [
  "INFO",
  " ",
  "{",
  newField "server_id" (Just . quote $ server_id m),
  ",",
  newField "version" (Just . quote $ version m),
  ",",
  newField "go" (Just . quote $ go m),
  ",",
  newField "host" (Just . quote $ host m),
  ",",
  newField "port" (Just . packStr' . show . port $ m),
  ",",
  newField "max_payload" (Just . packStr' . show . max_payload $ m),
  ",",
  newField "proto" (Just . packStr' . show . proto $ m),
  maybeComma (client_id m),
  newField "client_id" (fmap (packStr' . show) . client_id $ m),
  maybeComma (auth_required m),
  newField "auth_required" (fmap boolToJSON . auth_required $ m),
  maybeComma (tls_required m),
  newField "tls_required" (fmap boolToJSON . tls_required $ m),
  maybeComma (connect_urls m),
  newField "connect_urls" (fmap arrayToJSON . connect_urls $ m),
  maybeComma (ldm m),
  newField "ldm" (fmap boolToJSON . ldm $ m),
  "}",
  "\r\n"
  ]

newField :: BS.ByteString -> Maybe BS.ByteString -> BS.ByteString
newField k v = case v of
  Nothing -> ""
  Just a  -> foldr BS.append "" [quote k, ":", a]

quote :: BS.ByteString -> BS.ByteString
quote bs = foldr BS.append "" ["\"", bs, "\""]

packStr' :: String -> BS.ByteString
packStr' = encodeUtf8 . T.pack

boolToJSON :: Bool -> BS.ByteString
boolToJSON b
  | b  = "true"
  | not b = "false"

commaSep :: [BS.ByteString] -> BS.ByteString
commaSep []     = ""
commaSep [x]    = foldr BS.append "" [quote x, commaSep []]
commaSep (x:xs) = foldr BS.append "" [quote x, ",", commaSep xs]

arrayToJSON :: [BS.ByteString] -> BS.ByteString
arrayToJSON bs = foldr BS.append "" ["[", commaSep bs, "]"]

maybeComma :: Maybe a -> BS.ByteString
maybeComma m = case m of
  Nothing -> ""
  Just _  -> ","

