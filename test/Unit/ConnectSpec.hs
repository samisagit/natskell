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
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Connect

spec :: Spec
spec = do
  generated
  explicit

boolCases = [True, False]
maybeUserCases = [Just "samisagit", Just "sam@google.com", Nothing]
maybePassCases = [Just "dsalkj09898(*)(UHJHI&*&*)(910", Nothing]
maybeNameCases = [Just "natskell-client",  Nothing]
maybeVersionCases = [Just "0.0.0", Just "v1.0.1", Just "13.0.0+123", Nothing]
maybeSigCases = [Just "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQSflKxwRJSMeKKF2QT4fwpMeJf36POk6yJVadQssw5c", Nothing]
maybeJwtCases = [Just "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c", Nothing]
maybeIntCases = [Just 1, Nothing]
maybeBoolCases = [Just True, Just False, Nothing]

generated = parallel $ do
  describe "generated" $ do
    describe "transformer" $ do
      forM_ boolCases $ \verbosity ->
        forM_ boolCases $ \pedanticity ->
          forM_ boolCases $ \tlsRequirement ->
            forM_ maybeJwtCases $ \authTokenOptions ->
              forM_ maybeUserCases $ \userOptions ->
                forM_ maybePassCases $ \passOptions ->
                  forM_ maybeNameCases $ \nameOptions ->
                    forM_ maybeIntCases $ \protocolOptions ->
                      forM_ maybeBoolCases $ \echoOptions ->
                        forM_ maybeSigCases $ \sigOptions ->
                          forM_ maybeJwtCases $ \jwtOptions -> do
                            let d = Connect verbosity pedanticity tlsRequirement authTokenOptions userOptions passOptions nameOptions "Haskell" 1 protocolOptions echoOptions sigOptions jwtOptions
                            it (printf "transforms %v successfully" (show d)) $ \f -> do
                              let transformed = transform d
                              let proto = BS.take 7 transformed
                              let json = BS.drop 8 transformed
                              let expectedFields = BS.init $ foldr BS.append "" [
                                    collapseMaybeStringField "auth_token" authTokenOptions,
                                    collapseMaybeBoolField "echo" echoOptions,
                                    collapseMaybeStringField "jwt" jwtOptions,
                                    collapseMaybeStringField "lang" (Just "Haskell"),
                                    collapseMaybeStringField "name" nameOptions,
                                    collapseMaybeStringField "pass" passOptions,
                                    collapseMaybeBoolField "pedantic" (Just pedanticity),
                                    collapseMaybeIntField "protocol" protocolOptions,
                                    collapseMaybeStringField "sig" sigOptions,
                                    collapseMaybeBoolField "tls_required" (Just tlsRequirement),
                                    collapseMaybeStringField "user" userOptions,
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

