{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Parser.API
    ( ParseStep (DropPrefix, Emit, NeedMore)
    , ParsedMessage (ParsedMsg, ParsedPing)
    , parse
    )
import           Parser.Attoparsec (parserApi)
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
