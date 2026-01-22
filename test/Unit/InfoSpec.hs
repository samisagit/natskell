{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module InfoSpec (spec) where

import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson         as A
import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Fixtures
import           GHC.Generics
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Types.Info

-- | Test data type for INFO with nonce
data InfoWithNonce = InfoWithNonce
  { iwn_server_id   :: T.Text
  , iwn_version     :: T.Text
  , iwn_go          :: T.Text
  , iwn_host        :: T.Text
  , iwn_port        :: Int
  , iwn_max_payload :: Int
  , iwn_proto       :: Int
  , iwn_nonce       :: T.Text
  }
  deriving (Eq, Generic, Show)

instance ToJSON InfoWithNonce where
  toJSON i = object
    [ "server_id"   .= iwn_server_id i
    , "version"     .= iwn_version i
    , "go"          .= iwn_go i
    , "host"        .= iwn_host i
    , "port"        .= iwn_port i
    , "max_payload" .= iwn_max_payload i
    , "proto"       .= iwn_proto i
    , "nonce"       .= iwn_nonce i
    ]

spec :: Spec
spec = do
  cases
  nonceSpec

nonceSpec :: Spec
nonceSpec = describe "nonce parsing" $ do
  it "parses info with nonce" $ do
    let testInfo = InfoWithNonce
          { iwn_server_id   = "test"
          , iwn_version     = "2.0"
          , iwn_go          = "go1.19"
          , iwn_host        = "0.0.0.0"
          , iwn_port        = 4222
          , iwn_max_payload = 1048576
          , iwn_proto       = 1
          , iwn_nonce       = "abc123nonce"
          }
    let json = A.encode testInfo
    let result = A.eitherDecode json :: Either String Info
    case result of
      Right info -> nonce info `shouldBe` Just "abc123nonce"
      Left e     -> expectationFailure e

explicitCases :: [(BS.ByteString, Info)]
explicitCases = [
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3, \"client_id\": 1, \"auth_required\": true, \"tls_required\": true, \"connect_urls\": [\"https://127.0.0.1:4222\"], \"ldm\": true, \"headers\": true}\r\n",
    Info "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 (Just 1) (Just True) (Just True) (Just ["https://127.0.0.1:4222"]) (Just True) (Just True) Nothing
  ),
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3}\r\n",
    Info "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  ),
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3, \"client_id\": 1, \"auth_required\": true, \"tls_required\": true, \"connect_urls\": [\"https://127.0.0.1:4222\", \"https://192.168.9.7:4222\"], \"ldm\": true, \"headers\": false}\r\n",
    Info "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 (Just 1) (Just True) (Just True) (Just ["https://127.0.0.1:4222", "https://192.168.9.7:4222"]) (Just True) (Just False) Nothing
  )
  ]

generatedCases :: [(BS.ByteString, Info)]
generatedCases = zip (map buildProtoInput infos) infos
  where
    infos = Info
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
          <*> maybeify boolCases
          <*> maybeify nonceCases

nonceCases :: [BS.ByteString]
nonceCases = ["abc123nonce", "xyzNONCE456"]

cases = parallel $ do
  describe "generic parser" $ do
    forM_ explicitCases $ \(input, want) ->
      it (printf "correctly parses explicit case %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Right (ParsedInfo want, "")
    forM_ generatedCases $ \(input, want) ->
      it (printf "correctly parses generated case %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Right (ParsedInfo want, "")

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
  maybeComma (headers m),
  newField "headers" (fmap boolToJSON . headers $ m),
  maybeComma (nonce m),
  newField "nonce" (fmap quote . nonce $ m),
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
boolToJSON b = if b then "true" else "false"

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

