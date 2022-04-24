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
        let result = fmap fst err
        result `shouldBe` Just Err
        let left = fmap snd err
        left `shouldBe` Just ""
  where
    err = runParser parser "-ERR 'Unknown Protocol Operation'\r\n"
