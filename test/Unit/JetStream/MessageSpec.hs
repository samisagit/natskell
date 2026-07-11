{-# LANGUAGE OverloadedStrings #-}

module JetStream.MessageSpec (spec) where

import           JetStream.Message.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe "status classification" $ do
    it "classifies no-message status headers" $ do
      classifyStatusHeaders (Just [("Status", "404"), ("Description", "No Messages")])
        `shouldBe` Just (PullNoMessages (Just "No Messages"))

    it "classifies request timeout status headers case-insensitively" $ do
      classifyStatusHeaders (Just [("status", "408"), ("description", "Request Timeout")])
        `shouldBe` Just (PullRequestTimeout (Just "Request Timeout"))

    it "preserves unrecognised status details" $ do
      classifyStatusHeaders (Just [("Status", "503"), ("Description", "Service Unavailable")])
        `shouldBe` Just (PullStatusError "503" (Just "Service Unavailable"))

    it "ignores normal message headers" $ do
      classifyStatusHeaders (Just [("Nats-Stream", "ORDERS")])
        `shouldBe` Nothing

  describe "ack payloads" $ do
    it "encodes ack verbs" $ do
      ackPayload Ack `shouldBe` "+ACK"
      nakPayload `shouldBe` "-NAK"
      inProgressPayload `shouldBe` "+WPI"
      termPayload `shouldBe` "+TERM"

  describe "pull request payloads" $ do
    it "encodes one-message requests with an expires timeout" $ do
      pullRequestPayload 1 defaultPullRequest
        `shouldBe` "{\"batch\":1,\"expires\":1000000000}"

    it "encodes no-wait requests without expires" $ do
      let request = defaultPullRequest { pullRequestNoWait = True }
      pullRequestPayload 1 request
        `shouldBe` "{\"batch\":1,\"no_wait\":true}"
