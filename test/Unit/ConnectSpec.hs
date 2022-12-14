{-# LANGUAGE OverloadedStrings #-}

module ConnectSpec (spec) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import           Data.Text                 (Text, pack)
import           Data.Text.Encoding
import qualified Docker.Client             as DC
import           Fixtures
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Connect

spec :: Spec
spec = do
  explicit

explicitCases :: [(BS.ByteString, Connect)]
explicitCases = [
  (
    "CONNECT {\"verbose\": false, \"pedantic\": false, \"tls_required\": false, \"lang\": \"Haskell\", \"version\": 1}",
    Connect False False False Nothing Nothing Nothing Nothing "Haskell" 1 Nothing Nothing Nothing Nothing
    ),
  (
    "CONNECT {\"verbose\": true, \"pedantic\": true, \"tls_required\": true, \"auth_token\": \"token\", \"user\": \"user\", \"pass\": \"pass\", \"name\": \"name\", \"lang\": \"Haskell\", \"version\": 1, \"protocol\": 3, \"echo\": true, \"sig\": \"sig\", \"jwt\": \"jwt\"}",
    Connect True True True (Just "token") (Just "user") (Just "pass") (Just "name") "Haskell" 1 (Just 3) (Just True) (Just "sig") (Just "jwt")
    )
  ]

explicit = parallel $ do
  describe "transformer" $ do
    forM_ explicitCases $ \(want, input) -> do
      it (printf "transforms %v successfully" (show input)) $ \f -> do

        let transformed = transform input

        let gotProto = BS.take 7 transformed
        let gotJSONString = BS.drop 8 transformed

        let wantJSON = decode . LBS.fromStrict $ BS.drop 8 want :: Maybe Value
        let gotJSON = decode . LBS.fromStrict $ gotJSONString :: Maybe Value

        gotProto `shouldBe` "CONNECT"
        gotJSON `shouldBe` wantJSON

