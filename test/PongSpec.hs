{-# LANGUAGE OverloadedStrings #-}

module PongSpec (spec) where

import           Parser
import           Pong
import           Test.Hspec

spec :: Spec
spec = do
  manual

manual :: Spec
manual = do
  describe "parser" $ do
    it "correctly parses PONG" $ do
      do
        let result = fmap fst pong
        result `shouldBe` Just Pong
        let left = fmap snd pong
        left `shouldBe` Just ""
  where
    pong = runParser parser "PONG\r\n"
