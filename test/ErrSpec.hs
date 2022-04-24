{-# LANGUAGE OverloadedStrings #-}

module ErrSpec (spec) where

import           Err
import           Parser
import           Test.Hspec

spec :: Spec
spec = do
  manual

manual :: Spec
manual = do
  describe "parser" $ do
    it "correctly parses -ERR" $ do
      do
        let result = fmap fst ping
        result `shouldBe` Just Err
        let left = fmap snd ping
        left `shouldBe` Just ""
  where
    ping = runParser parser "-ERR\r\n"
