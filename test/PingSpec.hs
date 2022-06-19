{-# LANGUAGE OverloadedStrings #-}

module PingSpec (spec) where

import           Parser
import           Ping
import           Test.Hspec

spec :: Spec
spec = do
  manual

manual :: Spec
manual = parallel $ do
  describe "parser" $ do
    it "correctly parses PING" $ do
      let result = fmap fst ping
      result `shouldBe` Just Ping
      let left = fmap snd ping
      left `shouldBe` Just ""
  describe "transformer" $ do
    it "correctly transforms to 'PING'" $ do
      transform Ping `shouldBe` "PING"
  where
    ping = runParser parser "PING\r\n"
