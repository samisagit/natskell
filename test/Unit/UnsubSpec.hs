{-# LANGUAGE OverloadedStrings #-}

module UnsubSpec (spec) where

import           Control.Monad
import           Data.ByteString
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Unsub

spec :: Spec
spec = do
  manual

cases :: [(Unsub, ByteString, String)]
cases = [
  (Unsub "uuid" Nothing, "UNSUB uuid", "without max messages"),
  (Unsub "uuid" (Just 10), "UNSUB uuid 10", "with max messages")
  ]

manual = parallel $ do
  forM_ cases $ \(input, expected, caseName) ->
    it (printf "correctly transforms %s" caseName) $ do
      transform input `shouldBe` expected
