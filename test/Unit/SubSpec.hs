{-# LANGUAGE OverloadedStrings #-}

module SubSpec (spec) where

import           Control.Monad
import           Data.ByteString
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Sub
import           Validators.Validators

spec :: Spec
spec = do
  transformerCases
  validateCases

explicitTransformerCases :: [(Sub, ByteString)]
explicitTransformerCases = [
  (Sub "SOME.SUBJ" Nothing "uuid", "SUB SOME.SUBJ uuid"),
  (Sub "SOME.SUBJ" (Just "QUEUE.GROUP.A") "uuid", "SUB SOME.SUBJ QUEUE.GROUP.A uuid")
  ]

transformerCases = parallel $ do
  describe "SUB transformer" $ do
    forM_ explicitTransformerCases $ \(input, want) ->
      it (printf "correctly transforms %s" (show input)) $ do
        transform input `shouldBe` want

explicitValidaterCases :: [(Sub, Either ByteString ())]
explicitValidaterCases = [
  (Sub ">" Nothing "a", Right ()),
  (Sub ">" (Just "1") "a", Right ()),
  (Sub "" Nothing "a", Left "explicit empty subject"),
  (Sub ">" (Just "") "a", Left "explicit empty queue group"),
  (Sub ">" Nothing "", Left "explicit empty sid")
  ]

validateCases = parallel $ do
  describe "SUB validater" $ do
    forM_ explicitValidaterCases $ \(input, want) ->
      it (printf "correctly validates %s" (show input)) $ do
        validate input `shouldBe` want

