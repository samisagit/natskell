{-# LANGUAGE OverloadedStrings #-}

module SubSpec (spec) where

import           Control.Monad
import           Data.ByteString
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Sub

spec :: Spec
spec = do
  cases

explicitCases :: [(Sub, ByteString)]
explicitCases = [
  (Sub "SOME.SUBJ" Nothing "uuid", "SUB SOME.SUBJ uuid"),
  (Sub "SOME.SUBJ" (Just "QUEUE.GROUP.A") "uuid", "SUB SOME.SUBJ QUEUE.GROUP.A uuid")
  ]

cases = parallel $ do
  describe "SUB transformer" $ do
    forM_ explicitCases $ \(input, want) ->
      it (printf "correctly transforms %s" (show input)) $ do
        transform input `shouldBe` want
