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
  cases

explicitCases :: [(Pub, BS.ByteString)]
explicitCases = [
  (Pub "FOO.BAR" Nothing Nothing, "PUB FOO.BAR 0\r\n"),
  (Pub "FOO.BAR" Nothing (Just "Some payload bits"), "PUB FOO.BAR 17\r\nSome payload bits\r\n"),
  (Pub "FOO.BAR" (Just "FOO.BAR.REPLY") Nothing, "PUB FOO.BAR FOO.BAR.REPLY 0\r\n"),
  (Pub "FOO.BAR" (Just "FOO.BAR.REPLY") (Just "Some payload bits"), "PUB FOO.BAR FOO.BAR.REPLY 17\r\nSome payload bits\r\n")
  ]

cases = parallel $ do
  describe "PUB transformer" $ do
    forM_ explicitCases $ \(input, want) ->
      it (printf "correctly transforms %s" (show input)) $ do
        transform input `shouldBe` want

