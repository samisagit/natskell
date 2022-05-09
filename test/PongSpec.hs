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
      let result = fmap fst pong
      result `shouldBe` Just Pong
      let left = fmap snd pong
      left `shouldBe` Just ""
  describe "transformer" $ do
    it "correctly transforms to 'PONG'" $ do
      transform Pong `shouldBe` "PONG"
  where
    pong = runParser parser "PONG\r\n"
