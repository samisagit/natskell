{-# LANGUAGE OverloadedStrings #-}

module OkSpec (spec) where

import           Lib.Parser
import           Parsers.Parsers
import           Test.Hspec
import           Types.Ok

spec :: Spec
spec = do
  manual

manual :: Spec
manual = parallel $ do
  describe "parser" $ do
    it "correctly parses +OK" $ do
      let output = runParser okParser "+OK\r\n"
      let result = fmap fst output
      let rest = fmap snd output
      case result of
        Just (ParsedOk a) -> a `shouldBe` Ok
        Nothing           -> error "parser did not return OK type"
      case rest of
        Just "" -> return ()
        _       -> error "parser did not consume all tokens"
