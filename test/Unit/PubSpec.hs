{-# LANGUAGE OverloadedStrings #-}

module PubSpec (spec) where

import           Control.Monad
import qualified Data.ByteString           as BS
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Pub

spec :: Spec
spec = do
  explicit

cases :: [(Pub, BS.ByteString, String)]
cases = [
  (Pub "FOO.BAR" Nothing Nothing, "PUB FOO.BAR 0\r\n", "without max messages"),
  (Pub "FOO.BAR" Nothing (Just "Some payload bits"), "PUB FOO.BAR 17\r\nSome payload bits\r\n", "with max messages"),
  (Pub "FOO.BAR" (Just "FOO.BAR.REPLY") Nothing, "PUB FOO.BAR FOO.BAR.REPLY 0\r\n", "with max messages"),
  (Pub "FOO.BAR" (Just "FOO.BAR.REPLY") (Just "Some payload bits"), "PUB FOO.BAR FOO.BAR.REPLY 17\r\nSome payload bits\r\n", "with max messages")
  ]

explicit = parallel $ do
  forM_ cases $ \(input, expected, caseName) ->
    it (printf "correctly transforms %s" caseName) $ do
      transform input `shouldBe` expected

