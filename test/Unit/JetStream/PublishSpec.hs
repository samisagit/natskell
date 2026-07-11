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
          , withExpectedLastSequence 12
          , withExpectedLastSubjectSequence 7
          , withExpectedLastMsgId "msg-0"
          , withHeaders [("Nats-Expected-Test", "custom")]
          ]
          `shouldBe` [ ("Nats-Msg-Id", "msg-1")
                     , ("Nats-Expected-Stream", "ORDERS")
                     , ("Nats-Expected-Last-Sequence", "12")
                     , ("Nats-Expected-Last-Subject-Sequence", "7")
                     , ("Nats-Expected-Last-Msg-Id", "msg-0")
                     , ("Nats-Expected-Test", "custom")
                     ]

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
