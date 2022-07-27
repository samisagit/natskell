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
  generated
  explicit


generated = parallel $ do
  describe "generated" $ do
    describe "transformer" $ do
      forM_ boolCases $ \verbosity ->
        forM_ boolCases $ \pedanticity ->
          forM_ boolCases $ \tlsRequirement ->
            forM_ (maybeify jwtCases) $ \authTokenOptions ->
              forM_ (maybeify userCases) $ \userOptions ->
                forM_ (maybeify passCases) $ \passOptions ->
                  forM_ (maybeify nameCases) $ \nameOptions ->
                    forM_ (maybeify intCases) $ \protocolOptions ->
                      forM_ (maybeify boolCases) $ \echoOptions ->
                        forM_ (maybeify sigCases) $ \sigOptions ->
                          forM_ (maybeify jwtCases) $ \jwtOptions -> do
                            let d = Connect verbosity pedanticity tlsRequirement (fmap pack authTokenOptions) (fmap pack userOptions) (fmap pack passOptions) (fmap pack nameOptions) "Haskell" 1 (fmap fromIntegral protocolOptions) echoOptions (fmap pack sigOptions) (fmap pack jwtOptions)
                            it (printf "transforms %v successfully" (show d)) $ \f -> do
                              let transformed = transform d
                              let proto = BS.take 7 transformed
                              let json = BS.drop 8 transformed
                              let expectedFields = BS.init $ foldr BS.append "" [
                                    collapseMaybeStringField "auth_token" $ fmap pack authTokenOptions,
                                    collapseMaybeBoolField "echo" echoOptions,
                                    collapseMaybeStringField "jwt" $ fmap pack jwtOptions,
                                    collapseMaybeStringField "lang" (Just "Haskell"),
                                    collapseMaybeStringField "name" $ fmap pack nameOptions,
                                    collapseMaybeStringField "pass" $ fmap pack passOptions,
                                    collapseMaybeBoolField "pedantic" (Just pedanticity),
                                    collapseMaybeIntField "protocol" $ fmap fromIntegral protocolOptions,
                                    collapseMaybeStringField "sig" $ fmap pack sigOptions,
                                    collapseMaybeBoolField "tls_required" (Just tlsRequirement),
                                    collapseMaybeStringField "user" $ fmap pack userOptions,
                                    collapseMaybeBoolField "verbose" (Just verbosity),
                                    collapseMaybeIntField "version" (Just 1)
                                    ]
                              -- we don't really care about field order, nor do we want to ensure it in the test
                              -- so we'll just decode the want and the got into a Haskell type and check they're equal
                              let want = (decode . LBS.fromStrict $ foldr BS.append "" ["{", expectedFields, "}"]) :: Maybe Value
                              let got = decode . LBS.fromStrict $ json :: Maybe Value
                              proto `shouldBe` "CONNECT"
                              got `shouldBe` want
explicitCases :: [Connect]
explicitCases = [
  Connect False False False Nothing Nothing Nothing Nothing "" 1 Nothing Nothing Nothing Nothing,
  Connect True True True (Just "token") (Just "user") (Just "pass") (Just "name") "Haskell" 1 (Just 3) (Just True) (Just "sig") (Just "jwt")
  ]

explicit = parallel $ do
  describe "transformer" $ do
    forM_ explicitCases $ \input -> do
      it (printf "transforms %v successfully" (show input)) $ \f -> do
        let transformed = transform input
        let proto = BS.take 7 transformed
        let json = BS.drop 8 transformed
        let expectedFields = BS.init $ foldr BS.append "" [
              collapseMaybeStringField "auth_token" $ auth_token input,
              collapseMaybeBoolField "echo" $ echo input,
              collapseMaybeStringField "jwt" $ jwt input,
              collapseMaybeStringField "lang" (Just $ lang input),
              collapseMaybeStringField "name" $ name input,
              collapseMaybeStringField "pass" $ pass input,
              collapseMaybeBoolField "pedantic" (Just $ pedantic input),
              collapseMaybeIntField "protocol" $ protocol input,
              collapseMaybeStringField "sig" $ sig input,
              collapseMaybeBoolField "tls_required" (Just $ tls_required input),
              collapseMaybeStringField "user" $ user input,
              collapseMaybeBoolField "verbose" (Just $ verbose input),
              collapseMaybeIntField "version" (Just $ version input)
              ]
        -- we don't really care about field order, nor do we want to ensure it in the test
        -- so we'll just decode the want and the got into a Haskell type and check they're equal
        let want = (decode . LBS.fromStrict $ foldr BS.append "" ["{", expectedFields, "}"]) :: Maybe Value
        let got = decode . LBS.fromStrict $ json :: Maybe Value
        proto `shouldBe` "CONNECT"
        got `shouldBe` want

