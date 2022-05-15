{-# LANGUAGE OverloadedStrings #-}

module ConnectSpec (spec) where

import           Connect
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString    as BS
import           Data.Text          (Text, pack)
import           Data.Text.Encoding
import           Parser
import           Test.Hspec
import           Text.Printf

spec :: Spec
spec = do
  manual

boolCases = [True, False]
maybeTextCases = [Just "some text", Nothing]
maybeIntCases = [Just 1, Nothing]
maybeBoolCases = [Just True, Just False, Nothing]

-- this relies on a deterministic order of JSON fields, which isn't ideal
manual = do
  describe "transformer" $ do
    forM_ boolCases $ \verbosity ->
      forM_ boolCases $ \pedanticity ->
        forM_ boolCases $ \tlsRequirement ->
          forM_ maybeTextCases $ \authTokenOptions ->
            forM_ maybeTextCases $ \userOptions ->
              forM_ maybeTextCases $ \passOptions ->
                forM_ maybeTextCases $ \nameOptions ->
                  forM_ maybeIntCases $ \protocolOptions ->
                    forM_ maybeBoolCases $ \echoOptions ->
                      forM_ maybeTextCases $ \sigOptions ->
                        forM_ maybeTextCases $ \jwtOptions -> do
                          let d = Data verbosity pedanticity tlsRequirement authTokenOptions userOptions passOptions nameOptions "Haskell" 1 protocolOptions echoOptions sigOptions jwtOptions
                          it (printf "transforms %v successfully" (show d)) $ \f -> do
                            let transformed = transform d
                            let proto = BS.take 7 transformed
                            let json = BS.drop 8 transformed
                            proto `shouldBe` "CONNECT"
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
                            json `shouldBe` foldr BS.append "" ["{", expectedFields, "}"]

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

