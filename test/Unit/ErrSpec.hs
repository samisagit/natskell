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
  ("-ERR 'Unknown Protocol Operation'\r\n", ErrUnknownOp "Unknown Protocol Operation"),
  ("-ERR 'Attempted To Connect To Route Port'\r\n", ErrRoutePortConn "Attempted To Connect To Route Port"),
  ("-ERR 'Authorization Violation'\r\n", ErrAuthViolation "Authorization Violation"),
  ("-ERR 'Authorization Timeout'\r\n", ErrAuthTimeout "Authorization Timeout"),
  ("-ERR 'Invalid Client Protocol'\r\n", ErrInvalidProtocol "Invalid Client Protocol"),
  ("-ERR 'Maximum Control Line Exceeded'\r\n", ErrMaxControlLineEx "Maximum Control Line Exceeded"),
  ("-ERR 'Secure Connection - TLS Required'\r\n", ErrTlsRequired "Secure Connection - TLS Required"),
  ("-ERR 'Stale Connection'\r\n", ErrStaleConn "Stale Connection"),
  ("-ERR 'Maximum Connections Exceeded'\r\n", ErrMaxConnsEx "Maximum Connections Exceeded"),
  ("-ERR 'Slow Consumer'\r\n", ErrSlowConsumer"Slow Consumer"),
  ("-ERR 'Maximum Payload Violation'\r\n", ErrMaxPayload "Maximum Payload Violation"),
  ("-ERR 'Invalid Subject'\r\n", ErrInvalidSubject "Invalid Subject"),
  ("-ERR 'Permissions Violation For Subscription To FOO.'\r\n", ErrPermViolation "Permissions Violation For Subscription To FOO."),
  ("-ERR 'Permissions Violation For Publish To FOO.'\r\n", ErrPermViolation "Permissions Violation For Publish To FOO.")
  ]

generatedCases =
  zip
    ((\x n-> foldr BS.append "" ["-ERR 'Permissions Violation For ", x, " To ", n, "'\r\n"]) <$> ["Subscription", "Publish"] <*> invalidSubjectCases)
    ((\x n-> ErrPermViolation $ foldr BS.append "" ["Permissions Violation For ", x, " To ", n]) <$> ["Subscription", "Publish"] <*> invalidSubjectCases)

spec :: Spec
spec = do
  cases

cases = parallel $ do
  describe "generic parser" $ do
    forM_ explicitCases $ \(input, want) ->
      it (printf "correctly parses explicit case %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Right (ParsedErr want, "")
    forM_ generatedCases $ \(input, want) ->
      it (printf "correctly parses generated case %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Right (ParsedErr want, "")

