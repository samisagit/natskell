{-# LANGUAGE OverloadedStrings #-}

module ErrSpec (spec) where

import           Control.Monad
import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           Fixtures
import           Lib.Parser
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Types.Err

cases :: [(BS.ByteString, Err)]
cases = [
  ("-ERR 'Unknown Protocol Operation'\r\n", Err "Unknown Protocol Operation" True),
  ("-ERR 'Attempted To Connect To Route Port'\r\n", Err "Attempted To Connect To Route Port" True),
  ("-ERR 'Authorization Violation'\r\n", Err "Authorization Violation" True),
  ("-ERR 'Authorization Timeout'\r\n", Err "Authorization Timeout" True),
  ("-ERR 'Invalid Client Protocol'\r\n", Err "Invalid Client Protocol" True),
  ("-ERR 'Maximum Control Line Exceeded'\r\n", Err "Maximum Control Line Exceeded" True),
  ("-ERR 'Parser Error'\r\n", Err "Parser Error" True),
  ("-ERR 'Secure Connection - TLS Required'\r\n", Err "Secure Connection - TLS Required" True),
  ("-ERR 'Stale Connection'\r\n", Err "Stale Connection" True),
  ("-ERR 'Maximum Connections Exceeded'\r\n", Err "Maximum Connections Exceeded" True),
  ("-ERR 'Slow Consumer'\r\n", Err "Slow Consumer" True),
  ("-ERR 'Maximum Payload Violation'\r\n", Err "Maximum Payload Violation" True),
  ("-ERR 'Invalid Subject'\r\n", Err "Invalid Subject" False),
  ("-ERR 'Permissions Violation For Subscription To FOO.'\r\n", Err "Permissions Violation For Subscription To FOO." False),
  ("-ERR 'Permissions Violation For Publish To FOO.'\r\n", Err "Permissions Violation For Publish To FOO." False)
  ]

spec :: Spec
spec = do
  explicit
  generated

explicit = parallel $ do
  describe "specific parser" $ do
    forM_ cases $ \(input, expected) ->
      it (printf "correctly parses %s" (show input)) $ do
        let output = runParser errParser input
        let result = fmap fst output
        let rest = fmap snd output
        case result of
          Just (ParsedErr a) -> a `shouldBe` expected
          Nothing            -> error "parser did not return ERR type"
        case rest of
          Just "" -> return ()
          _       -> error "parser did not consume all tokens"
  describe "generic parser" $ do
    forM_ cases $ \(input, expected) ->
      it (printf "correctly parses %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Just (ParsedErr expected)

generated = parallel $ do
  describe "generated" $ do
    describe "generic parser" $ do
      forM_ invalidSubjectCases $ \subject -> do
        let subInput = encodeUtf8 . T.pack $ "-ERR 'Permissions Violation For Subscription To " ++ subject ++ "'\r\n"
        let subExpected = Err (BS.init . BS.init . BS.init . BS.drop 6 $ subInput) False
        it (printf "correctly parses %s" (show subInput)) $ do
          let output = genericParse subInput
          output `shouldBe` Just (ParsedErr subExpected)
        let subInput = encodeUtf8 . T.pack $ "-ERR 'Permissions Violation For Publish To " ++ subject ++ "'\r\n"
        let subExpected = Err (BS.init . BS.init . BS.init . BS.drop 6 $ subInput) False
        it (printf "correctly parses %s" (show subInput)) $ do
          let output = genericParse subInput
          output `shouldBe` Just (ParsedErr subExpected)
