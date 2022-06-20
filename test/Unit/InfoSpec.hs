{-# LANGUAGE OverloadedStrings #-}

module InfoSpec (spec) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString as BS
import           Info
import           Parser
import           Test.Hspec
import           Text.Printf

spec :: Spec
spec = do
  manual

cases :: [(BS.ByteString, Data, String)]
cases = [
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3, \"client_id\": 1, \"auth_required\": true, \"tls_required\": true, \"connect_urls\": [\"https://127.0.0.1:4222\"], \"ldm\": true}\r\n",
    Data "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 (Just 1) True True (Just ["https://127.0.0.1:4222"]) True,
    "fully populated"
  ),
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3, \"auth_required\": true, \"tls_required\": true, \"ldm\": true}\r\n",
    Data "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 Nothing True True Nothing True,
    "minimal"
  ),
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3, \"client_id\": 1, \"auth_required\": true, \"tls_required\": true, \"connect_urls\": [\"https://127.0.0.1:4222\", \"https://192.168.9.7:4222\"], \"ldm\": true}\r\n",
    Data "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 (Just 1) True True (Just ["https://127.0.0.1:4222", "https://192.168.9.7:4222"]) True,
    "multi connect urls"
  )
  ]

manual = parallel $ do
  describe "parser" $ do
    forM_ cases $ \(input, expected, name) ->
      it (printf "parses %s case successfully" name) $ do
        let output = runParser parser input
        let result = fmap fst output
        let left = fmap snd output
        result `shouldBe` Just (Just expected)
        left `shouldBe` Just ""

