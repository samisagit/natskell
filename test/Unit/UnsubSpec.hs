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
  cases

explicitCases :: [(Unsub, ByteString)]
explicitCases = [
  (Unsub "uuid" Nothing, "UNSUB uuid"),
  (Unsub "uuid" (Just 10), "UNSUB uuid 10")
  ]

cases = parallel $ do
  describe "UNSUB transformer" $ do
    forM_ explicitCases $ \(input, want) ->
      it (printf "correctly transforms %s" (show input)) $ do
        transform input `shouldBe` want
