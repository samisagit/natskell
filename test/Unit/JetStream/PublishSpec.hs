{-# LANGUAGE OverloadedStrings #-}

module JetStream.PublishSpec (spec) where

import           Data.Aeson              (eitherDecodeStrict)
import           JetStream.Publish.Types
import           Test.Hspec

spec :: Spec
spec =
  parallel $ do
    describe "publishHeaders" $ do
      it "renders JetStream publish expectation headers" $ do
        publishHeaders
          [ withMsgId "msg-1"
          , withExpectedStream "ORDERS"
          , withPublishExpectation (ExpectedLastSequence 12)
          , withHeaders [("Nats-Expected-Test", "custom")]
          ]
          `shouldBe` [ ("Nats-Expected-Test", "custom")
                     , ("Nats-Expected-Last-Sequence", "12")
                     , ("Nats-Expected-Stream", "ORDERS")
                     , ("Nats-Msg-Id", "msg-1")
                     ]

      it "keeps publish expectations mutually exclusive" $ do
        publishHeaders
          [ withPublishExpectation (ExpectedLastSequence 12)
          , withPublishExpectation (ExpectedLastSubjectSequence 7)
          , withPublishExpectation (ExpectedLastMsgId "msg-0")
          ]
          `shouldBe` [("Nats-Expected-Last-Msg-Id", "msg-0")]

    describe "PublishAck" $ do
      it "decodes required and optional publish ack fields" $ do
        eitherDecodeStrict
          "{\"stream\":\"ORDERS\",\"seq\":42,\"duplicate\":true,\"domain\":\"hub\"}"
          `shouldBe` Right PublishAck
            { publishAckStream = "ORDERS"
            , publishAckSequence = 42
            , publishAckDuplicate = Just True
            , publishAckDomain = Just "hub"
            }

      it "decodes publish ack without optional fields" $ do
        eitherDecodeStrict "{\"stream\":\"ORDERS\",\"seq\":42}"
          `shouldBe` Right PublishAck
            { publishAckStream = "ORDERS"
            , publishAckSequence = 42
            , publishAckDuplicate = Nothing
            , publishAckDomain = Nothing
            }
