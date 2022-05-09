{-# LANGUAGE OverloadedStrings #-}

module OkSpec (spec) where

import           Ok
import           Parser
import           Test.Hspec

spec :: Spec
spec = do
  manual

manual :: Spec
manual = do
  describe "parser" $ do
    it "correctly parses +OK" $ do
      let result = fmap fst ok
      result `shouldBe` Just Ok
      let left = fmap snd ok
      left `shouldBe` Just ""
  where
    ok = runParser parser "+OK\r\n"
