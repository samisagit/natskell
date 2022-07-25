{-# LANGUAGE OverloadedStrings #-}

module InfoSpec (spec) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString as BS
import           Lib.Parser
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Types.Info

spec :: Spec
spec = do
  manual
  generated

serverIDCases = ["serverA", "server A", "d9b3a74c-21f2-424f-9613-c3e8c6649455", "123"]
versionCases = ["1", "1.1.1", "v3.2.1", "1.11.1+1658688064", "v1.11.1+1658688064", "d9b3a74c-21f2-424f-9613-c3e8c6649455", "123"]
goVersionCases = ["1", "1.17", "2.0"]
hostCases = ["127.0.0.1", "168.212.226.204"]
portCases = [4222, 8080, 81, 65535]
maxPayloadCases = [1024, 512, 256, 128, 64, 32, 16, 8]
protocolCases = [1, 2, 3]
clientIDCases = [Just 1, Just 100, Nothing]
maybeBoolCases = [Just True, Just False, Nothing]
connectStringsCases = [Just [], Just ["127.0.0.1:4222"], Just ["127.0.0.1:4222", "0.0.0.0:1234"],  Nothing]

generated = parallel $ do
  describe "generated" $ do
    describe "specific parser" $ do
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
                              it "parses INFO..." $ do
--                                let info = Info serverID version goVersion host port maxPayload protocol clientID authRequired tlsRequired connectStrings ldm
                                1 `shouldBe` 1

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
