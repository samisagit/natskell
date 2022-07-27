{-# LANGUAGE OverloadedStrings #-}

module InfoSpec (spec) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString    as BS
import           Data.Text          (Text, pack)
import           Data.Text.Encoding
import           Lib.Parser
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Types.Info

spec :: Spec
spec = do
  manual
  generated

serverIDCases = ["d9b3a74c-21f2-424f-9613-c3e8c6649455", "123"]
versionCases = ["v3.2.1", "v1.11.1+1658688064"]
goVersionCases = ["1.17"]
hostCases = ["168.212.226.204"]
portCases = [4222]
maxPayloadCases = [1024]
protocolCases = [1, 2, 3]
clientIDCases = [Just 1, Just 100, Nothing]
maybeBoolCases = [Just True, Just False, Nothing]
connectStringsCases = [Just [], Just ["127.0.0.1:4222"], Just ["127.0.0.1:4222", "0.0.0.0:1234"],  Nothing]

generated = parallel $ do
  describe "generated" $ do
    describe "generic parser" $ do
      forM_ serverIDCases $ \serverID ->
        forM_ versionCases $ \version ->
          forM_ goVersionCases $ \goVersion ->
            forM_ hostCases $ \ host ->
              forM_ portCases $ \port ->
                forM_ maxPayloadCases $ \maxPayload ->
                  forM_ protocolCases $ \protocol ->
                    forM_ clientIDCases $ \clientID ->
                      forM_ maybeBoolCases $ \authRequired ->
                        forM_ maybeBoolCases $ \tlsRequired ->
                          forM_ connectStringsCases $ \connectStrings ->
                            forM_ maybeBoolCases $ \ldm -> do
                              let inputFields = BS.init $ foldr BS.append "" [
                                   collapseMaybeStringField "server_id" (Just serverID),
                                   collapseMaybeStringField "version" (Just version),
                                   collapseMaybeStringField "go" (Just goVersion),
                                   collapseMaybeStringField "host" (Just host),
                                   collapseMaybeIntField "port" (Just port),
                                   collapseMaybeIntField "max_payload" (Just maxPayload),
                                   collapseMaybeIntField "proto" (Just protocol),
                                   collapseMaybeIntField "client_id" clientID,
                                   collapseMaybeBoolField "auth_required" authRequired,
                                   collapseMaybeBoolField "tls_required" tlsRequired,
                                   collapseMaybeStringListField "connect_urls" connectStrings,
                                   collapseMaybeBoolField "ldm" ldm
                                   ]
                              let input = foldr BS.append "" ["INFO {", inputFields, "}\r\n"]
                              it (printf "parses %s successfully" (show input)) $ \f -> do
                                let expected = Info serverID version goVersion host port maxPayload protocol clientID authRequired tlsRequired connectStrings ldm
                                let output = genericParse input
                                output `shouldBe` Just (ParsedInfo expected)

cases :: [(BS.ByteString, Info, String)]
cases = [
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3, \"client_id\": 1, \"auth_required\": true, \"tls_required\": true, \"connect_urls\": [\"https://127.0.0.1:4222\"], \"ldm\": true}\r\n",
    Info "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 (Just 1) (Just True) (Just True) (Just ["https://127.0.0.1:4222"]) (Just True),
    "fully populated"
  ),
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3, \"auth_required\": true, \"tls_required\": true, \"ldm\": true}\r\n",
    Info "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 Nothing (Just True) (Just True) Nothing (Just True),
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

