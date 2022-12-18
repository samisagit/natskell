{-# LANGUAGE OverloadedStrings #-}

module ErrSpec (spec) where

import           Control.Monad
import qualified Data.ByteString as BS
import           Fixtures
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Types.Err

explicitCases :: [(BS.ByteString, Err)]
explicitCases = [
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

generatedCases =
  zip
    ((\x n-> foldr BS.append "" ["-ERR 'Permissions Violation For ", x, " To ", n, "'\r\n"]) <$> ["Subscription", "Publish"] <*> invalidSubjectCases)
    ((\x n-> Err $ foldr BS.append "" ["Permissions Violation For ", x, " To ", n]) <$> ["Subscription", "Publish"] <*> invalidSubjectCases <*> [False])

spec :: Spec
spec = do
  cases

cases = parallel $ do
  describe "generic parser" $ do
    forM_ explicitCases $ \(input, want) ->
      it (printf "correctly parses explicit case %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Just (ParsedErr want)
    forM_ generatedCases $ \(input, want) ->
      it (printf "correctly parses generated case %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Just (ParsedErr want)

