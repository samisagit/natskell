{-# LANGUAGE OverloadedStrings #-}

module PubSpec (spec) where

import           Control.Monad
import qualified Data.ByteString           as BS
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Pub
import           Validators.Validators

spec :: Spec
spec = do
  transformerCases
  validateCases

explicitTransformerCases :: [(Pub, BS.ByteString)]
explicitTransformerCases = [
  (Pub "FOO.BAR" Nothing Nothing, "PUB FOO.BAR 0\r\n"),
  (Pub "FOO.BAR" Nothing (Just "Some payload bits"), "PUB FOO.BAR 17\r\nSome payload bits\r\n"),
  (Pub "FOO.BAR" (Just "FOO.BAR.REPLY") Nothing, "PUB FOO.BAR FOO.BAR.REPLY 0\r\n"),
  (Pub "FOO.BAR" (Just "FOO.BAR.REPLY") (Just "Some payload bits"), "PUB FOO.BAR FOO.BAR.REPLY 17\r\nSome payload bits\r\n")
  ]

transformerCases = parallel $ do
  describe "PUB transformer" $ do
    forM_ explicitTransformerCases $ \(input, want) ->
      it (printf "correctly transforms %s" (show input)) $ do
        transform input `shouldBe` want

explicitValidaterCases :: [(Pub, Maybe BS.ByteString)]
explicitValidaterCases = [
  (Pub ">" Nothing Nothing, Nothing),
  (Pub ">" (Just "SUB.ME") Nothing, Nothing),
  (Pub ">" Nothing (Just "payload"), Nothing),
  (Pub ">" (Just "SUB.ME") (Just "payload"), Nothing),
  (Pub "" Nothing Nothing, Just "explicit empty subject"),
  (Pub ">" (Just "") Nothing, Just "explicit empty replyTo"),
  (Pub ">" Nothing (Just ""), Just "explicit empty payload")
  ]

validateCases = parallel $ do
  describe "PUB validater" $ do
    forM_ explicitValidaterCases $ \(input, want) ->
      it (printf "correctly validates %s" (show input)) $ do
        validate input `shouldBe` want

