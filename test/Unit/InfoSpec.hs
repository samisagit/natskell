{-# LANGUAGE OverloadedStrings #-}

module InfoSpec (spec) where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString   as BS
import           Lib.Parser
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Types.Info

spec :: Spec
spec = do
  manual
--  generated
--
--generated = parallel $ do
--  describe "generated" $ do
--    describe "generic parser" $ do
--      forM_ serverIDCases $ \serverID ->
--        forM_ versionCases $ \version ->
--          forM_ goVersionCases $ \goVersion ->
--            forM_ hostCases $ \ host ->
--              forM_ portCases $ \port ->
--                forM_ maxPayloadCases $ \maxPayload ->
--                  forM_ protocolCases $ \protocol ->
--                    forM_ clientIDCases $ \clientID ->
--                      forM_ (maybeify boolCases) $ \authRequired ->
--                        forM_ (maybeify boolCases) $ \tlsRequired ->
--                          forM_ (maybeify connectStringCases) $ \connectStrings ->
--                            forM_ (maybeify boolCases )$ \ldm -> do
--                              let inputFields = BS.init $ foldr BS.append "" [
----                                   collapseMaybeStringField "server_id" (Just $ pack serverID),
----                                   collapseMaybeStringField "version" (Just $ pack version),
----                                   collapseMaybeStringField "go" (Just $ pack goVersion),
----                                   collapseMaybeStringField "host" (Just $ pack host),
----                                   collapseMaybeIntField "port" (Just $ fromIntegral port),
----                                   collapseMaybeIntField "max_payload" (Just $ fromIntegral maxPayload),
----                                   collapseMaybeIntField "proto" (Just $ fromIntegral protocol),
----                                   collapseMaybeIntField "client_id" $ Just $ fromIntegral clientID,
----                                   collapseMaybeBoolField "auth_required" authRequired,
----                                   collapseMaybeBoolField "tls_required" tlsRequired,
----                                   collapseMaybeStringListField "connect_urls" connectStrings,
----                                   collapseMaybeBoolField "ldm" ldm
--                                   ]
--                              let input = foldr BS.append "" ["INFO {", inputFields, "}\r\n"]
--                              it (printf "parses %s successfully" (show input)) $ \f -> do
--                                let expected = Info serverID version goVersion host port maxPayload protocol (Just clientID) authRequired tlsRequired connectStrings ldm
--                                let output = genericParse input
--                                output `shouldBe` Just (ParsedInfo expected)

cases :: [(BS.ByteString, Info, String)]
cases = [
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3, \"client_id\": 1, \"auth_required\": true, \"tls_required\": true, \"connect_urls\": [\"https://127.0.0.1:4222\"], \"ldm\": true}\r\n",
    Info "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 (Just 1) (Just True) (Just True) (Just ["https://127.0.0.1:4222"]) (Just True),
    "fully populated"
  ),
  (
    "INFO {\"server_id\": \"some-server\", \"version\": \"semver\", \"go\": \"1.13\", \"host\": \"127.0.0.1\", \"port\": 4222, \"max_payload\": 1024, \"proto\": 3}\r\n",
    Info "some-server" "semver" "1.13" "127.0.0.1" 4222 1024 3 Nothing Nothing Nothing Nothing Nothing,
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
        output' <- try (evaluate (runParser infoParser input)) :: IO (Either SomeException (Maybe (ParsedMessage, BS.ByteString)))
        case output' of
          Left ex -> error $ show ex
          Right m -> return ()
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
