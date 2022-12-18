{-# LANGUAGE OverloadedStrings #-}

module ConnectSpec (spec) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Connect

spec :: Spec
spec = do
  cases

explicitCases :: [(Connect, BS.ByteString)]
explicitCases = [
  (
    Connect False False False Nothing Nothing Nothing Nothing "Haskell" "1" Nothing Nothing Nothing Nothing,
    "CONNECT {\"verbose\": false, \"pedantic\": false, \"tls_required\": false, \"lang\": \"Haskell\", \"version\": \"1\"}"
    ),
  (
    Connect True True True (Just "token") (Just "user") (Just "pass") (Just "name") "Haskell" "1" (Just 3) (Just True) (Just "sig") (Just "jwt"),
    "CONNECT {\"verbose\": true, \"pedantic\": true, \"tls_required\": true, \"auth_token\": \"token\", \"user\": \"user\", \"pass\": \"pass\", \"name\": \"name\", \"lang\": \"Haskell\", \"version\": \"1\", \"protocol\": 3, \"echo\": true, \"sig\": \"sig\", \"jwt\": \"jwt\"}"
    ),
  (
    Connect True True True (Just "token") (Just "user") (Just "pass") (Just "name") "Haskell" "1.0.0" (Just 3) (Just True) (Just "sig") (Just "jwt"),
    "CONNECT {\"verbose\": true, \"pedantic\": true, \"tls_required\": true, \"auth_token\": \"token\", \"user\": \"user\", \"pass\": \"pass\", \"name\": \"name\", \"lang\": \"Haskell\", \"version\": \"1.0.0\", \"protocol\": 3, \"echo\": true, \"sig\": \"sig\", \"jwt\": \"jwt\"}"
    )
  ]

cases = parallel $ do
  describe "CONNECT transformer" $ do
    forM_ explicitCases $ \(input, want) -> do
      it (printf "correctly transforms %s" (show input)) $ do
        let wantProto = BS.take 8 want
        let wantJSONString = BS.drop 8 want

        let transformed = transform input
        let gotProto = BS.take 8 transformed
        let gotJSONString = BS.drop 8 transformed

        -- decode both to avoid field ordering issues
        let wantJSON = decode . LBS.fromStrict $ wantJSONString :: Maybe Value
        let gotJSON = decode . LBS.fromStrict $ gotJSONString :: Maybe Value

        gotProto `shouldBe` wantProto
        gotJSON `shouldBe` wantJSON

