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
  manual

cases :: [(Sub, ByteString, String)]
cases = [
  (Sub "SOME.SUBJ" Nothing "uuid", "SUB SOME.SUBJ uuid", "without queue group"),
  (Sub "SOME.SUBJ" (Just "QUEUE.GROUP.A") "uuid", "SUB SOME.SUBJ QUEUE.GROUP.A uuid", "with queue group")
  ]

manual = parallel $ do
  forM_ cases $ \(input, expected, caseName) ->
    it (printf "correctly transforms %s" caseName) $ do
      transform input `shouldBe` expected
