{-# LANGUAGE OverloadedStrings #-}

module MsgSpec (spec) where

import Msg
import Parser
import Test.Hspec

spec :: Spec
spec = do
  manual

manual :: Spec
manual = do
  describe "parse" $ do
    it "correctly parses MSG with reply-to and payload" $ do
      do
        let result = fmap fst withReplyAndPayload
        fmap subject result `shouldBe` Just "FOO"
        fmap sid result `shouldBe` Just 13
        fmap replyTo result `shouldBe` Just (Just "BAR")
        fmap byteCount result `shouldBe` Just 17
        fmap payload result `shouldBe` Just (Just "some payload bits")
    it "correctly parses MSG with reply-to " $ do
      do
        let result = fmap fst withReply
        fmap subject result `shouldBe` Just "FOO"
        fmap sid result `shouldBe` Just 13
        fmap replyTo result `shouldBe` Just (Just "BAR")
        fmap byteCount result `shouldBe` Just 10
        fmap payload result `shouldBe` Just Nothing
    it "correctly parses MSG with payload" $ do
      do
        let result = fmap fst withPayload
        fmap subject result `shouldBe` Just "FOO"
        fmap sid result `shouldBe` Just 13
        fmap replyTo result `shouldBe` Just Nothing
        fmap byteCount result `shouldBe` Just 17
        fmap payload result `shouldBe` Just (Just "some payload bits")
    it "correctly parses MSG with reply-to and payload" $ do
      do
        let result = fmap fst min
        fmap subject result `shouldBe` Just "FOO"
        fmap sid result `shouldBe` Just 13
        fmap replyTo result `shouldBe` Just Nothing
        fmap byteCount result `shouldBe` Just 10
        fmap payload result `shouldBe` Just Nothing
  where
    withReplyAndPayload = runParser parser "MSG FOO 13 BAR 17\r\nsome payload bits\r\n"
    withReply = runParser parser "MSG FOO 13 BAR 10\r\n"
    withPayload = runParser parser "MSG FOO 13 17\r\nsome payload bits\r\n"
    min = runParser parser "MSG FOO 13 10\r\n"
