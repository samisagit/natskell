{-# LANGUAGE OverloadedStrings #-}

module PubSpec (spec) where

import           Control.Monad
import           Data.ByteString
import           Pub
import           Test.Hspec
import           Text.Printf

spec :: Spec
spec = do
  manual

cases :: [(Data, ByteString, String)]
cases = [
  (Data "FOO.BAR" Nothing Nothing, "PUB FOO.BAR 0\r\n", "without max messages"),
  (Data "FOO.BAR" Nothing (Just "Some payload bits"), "PUB FOO.BAR 17\r\nSome payload bits\r\n", "with max messages"),
  (Data "FOO.BAR" (Just "FOO.BAR.REPLY") Nothing, "PUB FOO.BAR FOO.BAR.REPLY 0\r\n", "with max messages"),
  (Data "FOO.BAR" (Just "FOO.BAR.REPLY") (Just "Some payload bits"), "PUB FOO.BAR FOO.BAR.REPLY 17\r\nSome payload bits\r\n", "with max messages")
  ]

manual = parallel $ do
  forM_ cases $ \(input, expected, caseName) ->
    it (printf "correctly transforms %s" caseName) $ do
      transform input `shouldBe` expected
