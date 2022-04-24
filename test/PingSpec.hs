{-# LANGUAGE OverloadedStrings #-}

module PingSpec (spec) where

import           Parser
import           Ping
import           Test.Hspec

spec :: Spec
spec = do
  manual

manual :: Spec
manual = do
  describe "parser" $ do
    it "correctly parses PING" $ do
      do
        let result = fmap fst ping
        result `shouldBe` Just Ping
        let left = fmap snd ping
        left `shouldBe` Just ""
  where
    ping = runParser parser "PING\r\n"
