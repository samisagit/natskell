{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Parser.API
    ( ParseStep (DropPrefix, Emit, NeedMore)
    , ParsedMessage (ParsedMessageTooLarge, ParsedMsg, ParsedPing)
    , parse
    )
import           Parser.Attoparsec (parserApi, parserApiWithMessageLimit)
import           Test.Hspec
import qualified Types.Msg         as Msg
import           Types.Ping        (Ping (Ping))

spec :: Spec
spec = do
  describe "parser api" $ do
    it "parses a valid frame through the API" $ do
      parse parserApi "PING\r\n" `shouldBe` Emit (ParsedPing Ping) ""

    it "suggests pulling more data for truncated input" $ do
      parse parserApi "MSG FOO 1 5\r\nHEL" `shouldBe` NeedMore

    it "rejects an oversized MSG before waiting for its payload" $ do
      parse (parserApiWithMessageLimit 4) "MSG FOO 1 5\r\n"
        `shouldBe` Emit (ParsedMessageTooLarge 5 4) ""

    it "rejects an oversized HMSG before waiting for its body" $ do
      parse (parserApiWithMessageLimit 15) "HMSG FOO 1 12 16\r\n"
        `shouldBe` Emit (ParsedMessageTooLarge 16 15) ""

    it "accepts a message exactly at the configured limit" $ do
      parse (parserApiWithMessageLimit 5) "MSG FOO 1 5\r\nHELLO\r\n"
        `shouldBe`
          Emit
            (ParsedMsg (Msg.Msg "FOO" "1" Nothing (Just "HELLO") Nothing))
            ""

    it "suggests dropping invalid prefix bytes" $ do
      case parse parserApi "LOL" of
        DropPrefix n _ ->
          n `shouldSatisfy` (> 0)
        other ->
          expectationFailure ("expected DropPrefix, got " ++ show other)

  describe "message parsing" $ do
    it "accepts tab-delimited fields and non-alphanumeric subjects" $ do
      let input = "MSG foo-bar\t1\t_INBOX.a_b\t5\r\nHELLO\r\n"
      case parse parserApi input of
        Emit parsed rest -> do
          rest `shouldBe` ""
          case parsed of
            ParsedMsg msg' -> do
              Msg.subject msg' `shouldBe` "foo-bar"
              Msg.replyTo msg' `shouldBe` Just "_INBOX.a_b"
              Msg.payload msg' `shouldBe` Just "HELLO"
            other ->
              expectationFailure ("unexpected parse result: " ++ show other)
        other ->
          expectationFailure ("unexpected parse result: " ++ show other)

    it "trims horizontal whitespace in HMSG headers" $ do
      let input =
            "HMSG FOO 13 30 35\r\nNATS/1.0\r\n  KEY  : VALUE  \r\n\r\nHELLO\r\n"
      case parse parserApi input of
        Emit parsed rest -> do
          rest `shouldBe` ""
          case parsed of
            ParsedMsg msg' -> do
              Msg.headers msg' `shouldBe` Just [("KEY", "VALUE")]
              Msg.payload msg' `shouldBe` Just "HELLO"
            other ->
              expectationFailure ("unexpected parse result: " ++ show other)
        other ->
          expectationFailure ("unexpected parse result: " ++ show other)

    it "exposes inline HMSG status and description as headers" $ do
      let input =
            "HMSG FOO 13 28 28\r\nNATS/1.0 404 No Messages\r\n\r\n\r\n"
      case parse parserApi input of
        Emit parsed rest -> do
          rest `shouldBe` ""
          case parsed of
            ParsedMsg msg' -> do
              Msg.headers msg' `shouldBe` Just [("Status", "404"), ("Description", "No Messages")]
              Msg.payload msg' `shouldBe` Just ""
            other ->
              expectationFailure ("unexpected parse result: " ++ show other)
        other ->
          expectationFailure ("unexpected parse result: " ++ show other)

    it "exposes inline HMSG status without a description" $ do
      let input =
            "HMSG FOO 13 16 16\r\nNATS/1.0 404\r\n\r\n\r\n"
      case parse parserApi input of
        Emit parsed rest -> do
          rest `shouldBe` ""
          case parsed of
            ParsedMsg msg' -> do
              Msg.headers msg' `shouldBe` Just [("Status", "404")]
              Msg.payload msg' `shouldBe` Just ""
            other ->
              expectationFailure ("unexpected parse result: " ++ show other)
        other ->
          expectationFailure ("unexpected parse result: " ++ show other)

    it "rejects malformed inline HMSG statuses" $ do
      let input =
            "HMSG FOO 13 17 17\r\nNATS/1.0 404x\r\n\r\n\r\n"
      case parse parserApi input of
        DropPrefix n reason -> do
          n `shouldSatisfy` (> 0)
          reason `shouldContain` "invalid HMSG status"
        other ->
          expectationFailure ("unexpected parse result: " ++ show other)

    it "rejects HMSG frames whose total size is smaller than the header size" $ do
      let input = "HMSG FOO 13 24 23\r\n"
      case parse parserApi input of
        DropPrefix n reason -> do
          n `shouldSatisfy` (> 0)
          reason `shouldContain` "invalid HMSG sizes"
        other ->
          expectationFailure ("unexpected parse result: " ++ show other)

    it "rejects HMSG headers with an empty key after trimming" $ do
      let input =
            "HMSG FOO 13 23 23\r\nNATS/1.0\r\n  : VALUE\r\n\r\n\r\n"
      case parse parserApi input of
        DropPrefix n reason -> do
          n `shouldSatisfy` (> 0)
          reason `shouldContain` "invalid HMSG headers"
        other ->
          expectationFailure ("unexpected parse result: " ++ show other)
