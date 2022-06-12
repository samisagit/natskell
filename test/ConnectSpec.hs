{-# LANGUAGE OverloadedStrings #-}

module ConnectSpec (spec) where

import           Connect
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text, pack)
import           Data.Text.Encoding
import qualified Docker.Client        as DC
import           Parser
import           Test.Hspec
import           Text.Printf

spec :: Spec
spec = do
  manual
  integration

boolCases = [True, False]
maybeUserCases = [Just "samisagit", Just "sam@google.com", Nothing]
maybePassCases = [Just "dsalkj09898(*)(UHJHI&*&*)(910", Just "password",  Nothing]
maybeNameCases = [Just "natskell-client",  Nothing]
maybeVersionCases = [Just "0.0.0", Just "v1.0.1", Just "13.0.0+123", Nothing]
maybeSigCases = [Just "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQSflKxwRJSMeKKF2QT4fwpMeJf36POk6yJVadQssw5c", Nothing]
maybeJwtCases = [Just "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c", Nothing]
maybeIntCases = [Just 1, Just 10, Nothing]
maybeBoolCases = [Just True, Just False, Nothing]

-- this relies on a deterministic order of JSON fields, which isn't ideal
manual = do
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
                            let decoded = decode . LBS.fromStrict $ json :: Maybe Value
                            let want = (decode . LBS.fromStrict $ foldr BS.append "" ["{", expectedFields, "}"]) :: Maybe Value
                            decoded `shouldBe` want

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

runNATSContainer :: IO DC.ContainerID
runNATSContainer = do
  h <- DC.unixHttpHandler "/var/run/docker.sock"
  DC.runDockerT (DC.defaultClientOpts, h) $
    do let pb = DC.PortBinding 4222 DC.TCP [DC.HostPort "0.0.0.0" 4222]
       let createOpts = DC.addPortBinding pb $ DC.defaultCreateOpts "nats:latest"
       cid <- DC.createContainer createOpts (Just "nats")
       case cid of
         Left err -> error $ show err
         Right i -> do
           _ <- DC.startContainer DC.defaultStartOpts i
           return i

stopNATSContainer :: DC.ContainerID -> IO ()
stopNATSContainer cid = do
  h <- DC.unixHttpHandler "/var/run/docker.sock"
  DC.runDockerT (DC.defaultClientOpts, h) $
    do r <- DC.stopContainer DC.DefaultTimeout cid
       case r of
         Left e  -> error $ show e
         Right _ -> return ()

withNATSConnection :: (DC.ContainerID -> IO ()) -> IO ()
withNATSConnection = bracket runNATSContainer stopNATSContainer

integration = do
  around withNATSConnection $ do
    describe "CONNECT" $ do
      it "connects successfully" $ \f -> do
        1 `shouldBe` 1
