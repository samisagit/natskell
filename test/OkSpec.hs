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
      do
        let result = fmap fst ping
        result `shouldBe` Just Ok
        let left = fmap snd ping
        left `shouldBe` Just ""
  where
    ping = runParser parser "+OK\r\n"
