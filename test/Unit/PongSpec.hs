{-# LANGUAGE OverloadedStrings #-}

module PongSpec (spec) where

import           Lib.Parser
import           Parsers.Parsers
import           Test.Hspec
import           Transformers.Transformers
import           Types.Pong

spec :: Spec
spec = do
  manual

manual :: Spec
manual = parallel $ do
  describe "specific parser" $ do
    it "correctly parses PONG" $ do
      let output = runParser pongParser "PONG\r\n"
      let result = fmap fst output
      let rest = fmap snd output
      case result of
        Just (ParsedPong a) -> a `shouldBe` Pong
        Nothing             -> error "parser did not return PONG type"
      case rest of
        Just "" -> return ()
        _       -> error "parser did not consume all tokens"
  describe "generic parser" $ do
    it "correctly parses PONG" $ do
      let output = genericParse "PONG\r\n"
      output `shouldBe` Just (ParsedPong Pong)
  describe "transformer" $ do
    it "correctly transforms to 'PONG'" $ do
      transform Pong `shouldBe` "PONG\r\n"
