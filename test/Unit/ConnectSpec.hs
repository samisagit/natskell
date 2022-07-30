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

explicitCases :: [Connect]
explicitCases = [
  Connect False False False Nothing Nothing Nothing Nothing "" 1 Nothing Nothing Nothing Nothing,
  Connect True True True (Just "token") (Just "user") (Just "pass") (Just "name") "Haskell" 1 (Just 3) (Just True) (Just "sig") (Just "jwt")
  ]

-- TODO: we should make this JSON explicit rather than building fields
explicit = parallel $ do
  describe "transformer" $ do
    forM_ explicitCases $ \input -> do
      it (printf "transforms %v successfully" (show input)) $ \f -> do
        let transformed = transform input
        let proto = BS.take 7 transformed
        let json = BS.drop 8 transformed
        let expectedFields = BS.init $ foldr BS.append "FIX ME " [
--              collapseMaybeStringField "auth_token" $ auth_token input,
--              collapseMaybeBoolField "echo" $ echo input,
--              collapseMaybeStringField "jwt" $ jwt input,
--              collapseMaybeStringField "lang" (Just $ lang input),
--              collapseMaybeStringField "name" $ name input,
--              collapseMaybeStringField "pass" $ pass input,
--              collapseMaybeBoolField "pedantic" (Just $ pedantic input),
--              collapseMaybeIntField "protocol" $ protocol input,
--              collapseMaybeStringField "sig" $ sig input,
--              collapseMaybeBoolField "tls_required" (Just $ tls_required input),
--              collapseMaybeStringField "user" $ user input,
--              collapseMaybeBoolField "verbose" (Just $ verbose input),
--              collapseMaybeIntField "version" (Just $ version input)
              ]
        -- we don't really care about field order, nor do we want to ensure it in the test
        -- so we'll just decode the want and the got into a Haskell type and check they're equal
        let want = (decode . LBS.fromStrict $ foldr BS.append "" ["{", expectedFields, "}"]) :: Maybe Value
        let got = decode . LBS.fromStrict $ json :: Maybe Value
        proto `shouldBe` "CONNECT"
        got `shouldBe` want

