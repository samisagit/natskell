{-# LANGUAGE OverloadedStrings #-}

module UnsubSpec (spec) where

import           Control.Monad
import           Data.ByteString
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Unsub
import           Validators.Validators

spec :: Spec
spec = do
  transformerCases
  validateCases

explicitTransformerCases :: [(Unsub, ByteString)]
explicitTransformerCases = [
  (Unsub "uuid" Nothing, "UNSUB uuid"),
  (Unsub "uuid" (Just 10), "UNSUB uuid 10")
  ]

transformerCases = parallel $ do
  describe "UNSUB transformer" $ do
    forM_ explicitTransformerCases $ \(input, want) ->
      it (printf "correctly transforms %s" (show input)) $ do
        transform input `shouldBe` want

explicitValidaterCases :: [(Unsub, Maybe ByteString)]
explicitValidaterCases = [
  (Unsub "a" Nothing, Nothing),
  (Unsub "a" (Just 1), Nothing),
  (Unsub "a" (Just 0), Nothing),
  (Unsub "" Nothing, Just "explicit empty sid")
  ]

validateCases = parallel $ do
  describe "UNSUB validater" $ do
    forM_ explicitValidaterCases $ \(input, want) ->
      it (printf "correctly validates %s" (show input)) $ do
        validate input `shouldBe` want

