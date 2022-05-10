{-# LANGUAGE OverloadedStrings #-}

module UnsubSpec (spec) where

import           Control.Monad
import           Data.ByteString
import           Test.Hspec
import           Text.Printf
import           Unsub

spec :: Spec
spec = do
  manual

cases :: [(Data, ByteString, String)]
cases = [
  (Data "uuid" Nothing, "UNSUB uuid", "without max messages"),
  (Data "uuid" (Just 10), "UNSUB uuid 10", "with max messages")
  ]

manual = do
  forM_ cases $ \(input, expected, caseName) ->
    it (printf "correctly transforms %s" caseName) $ do
      transform input `shouldBe` expected
