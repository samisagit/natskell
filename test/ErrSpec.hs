{-# LANGUAGE OverloadedStrings #-}

module ErrSpec (spec) where

import           Err
import           Parser
import           Test.Hspec

spec :: Spec
spec = do
  manual

manual :: Spec
manual = do
  describe "parser" $ do
    it "correctly parses -ERR 'Unknown Protocol Operation'" $ do
      do
        let result = fmap fst unknownOp
        result `shouldBe` Just (Err "Unknown Protocol Operation" True)
        let left = fmap snd unknownOp
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Attempted To Connect To Route Port'" $ do
      do
        let result = fmap fst routePortConn
        result `shouldBe` Just (Err "Attempted To Connect To Route Port" True)
        let left = fmap snd routePortConn
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Authorization Violation'" $ do
      do
        let result = fmap fst authViolation
        result `shouldBe` Just (Err "Authorization Violation" True)
        let left = fmap snd authViolation
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Authorization Timeout'" $ do
      do
        let result = fmap fst authTimeout
        result `shouldBe` Just (Err "Authorization Timeout" True)
        let left = fmap snd authTimeout
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Invalid Client Protocol'" $ do
      do
        let result = fmap fst invalidClientProtocol
        result `shouldBe` Just (Err "Invalid Client Protocol" True)
        let left = fmap snd invalidClientProtocol
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Maximum Control Line Exceeded'" $ do
      do
        let result = fmap fst maxControlLineExceeded
        result `shouldBe` Just (Err "Maximum Control Line Exceeded" True)
        let left = fmap snd maxControlLineExceeded
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Parser Error'" $ do
      do
        let result = fmap fst parseErr
        result `shouldBe` Just (Err "Parser Error" True)
        let left = fmap snd parseErr
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Secure Connection - TLS Required'" $ do
      do
        let result = fmap fst _TLSRequired
        result `shouldBe` Just (Err "Secure Connection - TLS Required" True)
        let left = fmap snd _TLSRequired
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Stale Connection'" $ do
      do
        let result = fmap fst staleConnection
        result `shouldBe` Just (Err "Stale Connection" True)
        let left = fmap snd staleConnection
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Maximum Connections Exceeded'" $ do
      do
        let result = fmap fst maxConnExceeded
        result `shouldBe` Just (Err "Maximum Connections Exceeded" True)
        let left = fmap snd maxConnExceeded
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Slow Consumer'" $ do
      do
        let result = fmap fst slowConsumer
        result `shouldBe` Just (Err "Slow Consumer" True)
        let left = fmap snd slowConsumer
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Maximum Payload Violation'" $ do
      do
        let result = fmap fst maxPayloadViolation
        result `shouldBe` Just (Err "Maximum Payload Violation" True)
        let left = fmap snd maxPayloadViolation
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Invalid Subject'" $ do
      do
        let result = fmap fst invalidSubj
        result `shouldBe` Just (Err "Invalid Subject" False)
        let left = fmap snd invalidSubj
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Permissions Violation for Subscription to FOO.'" $ do
      do
        let result = fmap fst subPermViolation
        result `shouldBe` Just (Err "Permissions Violation for Subscription to FOO." False)
        let left = fmap snd subPermViolation
        left `shouldBe` Just ""
    it "correctly parses -ERR 'Permissions Violation for Publish to FOO.'" $ do
      do
        let result = fmap fst pubPermViolation
        result `shouldBe` Just (Err "Permissions Violation for Publish to FOO." False)
        let left = fmap snd pubPermViolation
        left `shouldBe` Just ""
  where
    unknownOp = runParser parser "-ERR 'Unknown Protocol Operation'\r\n"
    routePortConn = runParser parser "-ERR 'Attempted To Connect To Route Port'\r\n"
    authViolation = runParser parser "-ERR 'Authorization Violation'\r\n"
    authTimeout = runParser parser "-ERR 'Authorization Timeout'\r\n"
    invalidClientProtocol = runParser parser "-ERR 'Invalid Client Protocol'\r\n"
    maxControlLineExceeded = runParser parser "-ERR 'Maximum Control Line Exceeded'\r\n"
    parseErr = runParser parser "-ERR 'Parser Error'\r\n"
    _TLSRequired = runParser parser "-ERR 'Secure Connection - TLS Required'\r\n"
    staleConnection = runParser parser "-ERR 'Stale Connection'\r\n"
    maxConnExceeded = runParser parser "-ERR 'Maximum Connections Exceeded'\r\n"
    slowConsumer = runParser parser "-ERR 'Slow Consumer'\r\n"
    maxPayloadViolation = runParser parser "-ERR 'Maximum Payload Violation'\r\n"
    invalidSubj = runParser parser "-ERR 'Invalid Subject'\r\n"
    subPermViolation = runParser parser "-ERR 'Permissions Violation for Subscription to FOO.'\r\n"
    pubPermViolation = runParser parser "-ERR 'Permissions Violation for Publish to FOO.'\r\n"

