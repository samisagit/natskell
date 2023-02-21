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
import           Validators.Validators

spec :: Spec
spec = do
  transformationCases
  validationCases

explicitTransformationCases :: [(Connect, BS.ByteString)]
explicitTransformationCases = [
  (
    Connect False False False Nothing Nothing Nothing Nothing "Haskell" "1" Nothing Nothing Nothing Nothing Nothing Nothing,
    "CONNECT {\"verbose\": false, \"pedantic\": false, \"tls_required\": false, \"lang\": \"Haskell\", \"version\": \"1\"}"
    ),
  (
    Connect True True True (Just "token") (Just "user") (Just "pass") (Just "name") "Haskell" "1" (Just 3) (Just True) (Just "sig") (Just "jwt") (Just True) (Just True),
    "CONNECT {\"verbose\": true, \"pedantic\": true, \"tls_required\": true, \"auth_token\": \"token\", \"user\": \"user\", \"pass\": \"pass\", \"name\": \"name\", \"lang\": \"Haskell\", \"version\": \"1\", \"protocol\": 3, \"echo\": true, \"sig\": \"sig\", \"jwt\": \"jwt\", \"no_responders\": true, \"headers\": true}"
    ),
  (
    Connect True True True (Just "token") (Just "user") (Just "pass") (Just "name") "Haskell" "1.0.0" (Just 3) (Just True) (Just "sig") (Just "jwt") (Just False) (Just False),
    "CONNECT {\"verbose\": true, \"pedantic\": true, \"tls_required\": true, \"auth_token\": \"token\", \"user\": \"user\", \"pass\": \"pass\", \"name\": \"name\", \"lang\": \"Haskell\", \"version\": \"1.0.0\", \"protocol\": 3, \"echo\": true, \"sig\": \"sig\", \"jwt\": \"jwt\", \"no_responders\": false, \"headers\": false}"
    )
  ]

transformationCases = parallel $ do
  describe "CONNECT transformer" $ do
    forM_ explicitTransformationCases $ \(input, want) -> do
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


explicitValidationCases :: [(Connect, Maybe BS.ByteString)]
explicitValidationCases = [
  (
    Connect False False False Nothing Nothing Nothing Nothing "Haskell" "1" Nothing Nothing Nothing Nothing Nothing Nothing,
    Nothing
  ),
  (
    Connect False False False (Just "") Nothing Nothing Nothing "Haskell" "1" Nothing Nothing Nothing Nothing Nothing Nothing,
    Just "explicit empty auth token"
  ),
  (
    Connect False False False Nothing (Just "") Nothing Nothing "Haskell" "1" Nothing Nothing Nothing Nothing Nothing Nothing,
    Just "explicit empty user"
  ),
  (
    Connect False False False Nothing Nothing (Just "") Nothing "Haskell" "1" Nothing Nothing Nothing Nothing Nothing Nothing,
    Just "explicit empty pass"
  ),
  (
    Connect False False False Nothing Nothing Nothing (Just "") "Haskell" "1" Nothing Nothing Nothing Nothing Nothing Nothing,
    Just "explicit empty name"
  ),
  (
    Connect False False False Nothing Nothing Nothing Nothing "" "1" Nothing Nothing Nothing Nothing Nothing Nothing,
    Just "explicit empty lang"
  ),
  (
    Connect False False False Nothing Nothing Nothing Nothing "Haskell" "" Nothing Nothing Nothing Nothing Nothing Nothing,
    Just "explicit empty version"
  ),
  (
    Connect False False False Nothing Nothing Nothing Nothing "Haskell" "1" (Just 0) Nothing Nothing Nothing Nothing Nothing,
    Nothing
  ),
  (
    Connect False False False Nothing Nothing Nothing Nothing "Haskell" "1" (Just 1) Nothing Nothing Nothing Nothing Nothing,
    Nothing
  ),
  (
    Connect False False False Nothing Nothing Nothing Nothing "Haskell" "1" (Just 2) Nothing Nothing Nothing Nothing Nothing,
    Just "invalid protocol"
  ),
  (
    Connect False False False Nothing Nothing Nothing Nothing "Haskell" "1" Nothing Nothing (Just "") Nothing Nothing Nothing,
    Just "explicit empty sig"
  ),
  (
    Connect False False False Nothing Nothing Nothing Nothing "Haskell" "1" Nothing Nothing Nothing (Just "") Nothing Nothing,
    Just "explicit empty jwt"
  )
  ]

validationCases = parallel $ do
  describe "CONNECT validater" $ do
    forM_ explicitValidationCases $ \(input, want) -> do
      it (printf "correctly validates %s" (show input)) $ do
        validate input `shouldBe` want

