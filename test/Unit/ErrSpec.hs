{-# LANGUAGE OverloadedStrings #-}

module ErrSpec (spec) where

import           Control.Monad
import           Data.ByteString
import           Lib.Parser
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Types.Err

cases :: [(ByteString, Err)]
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
  ("-ERR 'Permissions Violation For Subscription To >>>.*'\r\n", Err "Permissions Violation For Subscription To >>>.*" False),
  ("-ERR 'Permissions Violation For Subscription To nonsense*'\r\n", Err "Permissions Violation For Subscription To nonsense*" False),
  ("-ERR 'Permissions Violation For Publish To FOO.'\r\n", Err "Permissions Violation For Publish To FOO." False),
  ("-ERR 'Permissions Violation For Publish To >>>.*'\r\n", Err "Permissions Violation For Publish To >>>.*" False),
  ("-ERR 'Permissions Violation For Publish To nonsense*'\r\n", Err "Permissions Violation For Publish To nonsense*" False)
  ]

spec :: Spec
spec = do
  form

form = parallel $ do
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
