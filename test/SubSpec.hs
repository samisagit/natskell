{-# LANGUAGE OverloadedStrings #-}

module SubSpec (spec) where

import           Control.Monad
import           Data.ByteString
import           Sub
import           Test.Hspec
import           Text.Printf

spec :: Spec
spec = do
  manual

cases :: [(Data, ByteString, String)]
cases = [
  (Data "SOME.SUBJ" Nothing "uuid", "SUB SOME.SUBJ uuid", "without queue group"),
  (Data "SOME.SUBJ" (Just "QUEUE.GROUP.A") "uuid", "SUB SOME.SUBJ QUEUE.GROUP.A uuid", "with queue group")
  ]

manual = parallel $ do
  forM_ cases $ \(input, expected, caseName) ->
    it (printf "correctly transforms %s" caseName) $ do
      transform input `shouldBe` expected
