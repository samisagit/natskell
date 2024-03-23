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
  (Pub "FOO.BAR" Nothing Nothing Nothing, "PUB FOO.BAR 0\r\n\r\n"),
  (Pub "FOO.BAR" Nothing Nothing (Just "Some payload bits"), "PUB FOO.BAR 17\r\nSome payload bits\r\n"),
  (Pub "FOO.BAR" (Just "FOO.BAR.REPLY") Nothing Nothing, "PUB FOO.BAR FOO.BAR.REPLY 0\r\n\r\n"),
  (Pub "FOO.BAR" (Just "FOO.BAR.REPLY") Nothing (Just "Some payload bits"), "PUB FOO.BAR FOO.BAR.REPLY 17\r\nSome payload bits\r\n"),
  (Pub "FOO.BAR" Nothing (Just [("key", "value")]) Nothing, "HPUB FOO.BAR 23 23\r\nNATS/1.0\r\nkey:value\r\n\r\n\r\n"),
  (Pub "FOO.BAR" Nothing (Just [("key", "value"), ("foo", "bar")]) Nothing, "HPUB FOO.BAR 32 32\r\nNATS/1.0\r\nkey:value\r\nfoo:bar\r\n\r\n\r\n"),
  (Pub "FOO.BAR" (Just "FOO.BAR.REPLY") (Just [("key", "value")]) Nothing, "HPUB FOO.BAR FOO.BAR.REPLY 23 23\r\nNATS/1.0\r\nkey:value\r\n\r\n\r\n"),
  (Pub "FOO.BAR" Nothing (Just [("key", "value"), ("foo", "bar")]) (Just "Some payload bits"), "HPUB FOO.BAR 32 49\r\nNATS/1.0\r\nkey:value\r\nfoo:bar\r\n\r\nSome payload bits\r\n"),
  (Pub "FOO.BAR" (Just "FOO.BAR.REPLY") (Just [("key", "value"), ("foo", "bar")]) (Just "Some payload bits"), "HPUB FOO.BAR FOO.BAR.REPLY 32 49\r\nNATS/1.0\r\nkey:value\r\nfoo:bar\r\n\r\nSome payload bits\r\n")
  ]

transformerCases = parallel $ do
  describe "PUB transformer" $ do
    forM_ explicitTransformerCases $ \(input, want) ->
      it (printf "correctly transforms %s" (show input)) $ do
        transform input `shouldBe` want

explicitValidatorCases :: [(Pub, Either BS.ByteString ())]
explicitValidatorCases = [
  (Pub ">" Nothing Nothing Nothing, Right ()),
  (Pub ">" (Just "SUB.ME") Nothing Nothing, Right ()),
  (Pub ">" Nothing Nothing (Just "payload"), Right ()),
  (Pub ">" (Just "SUB.ME") Nothing (Just "payload"), Right ()),
  (Pub "" Nothing Nothing Nothing, Left "explicit empty subject"),
  (Pub ">" (Just "") Nothing Nothing, Left "explicit empty replyTo"),
  (Pub ">" Nothing Nothing (Just ""), Left "explicit empty payload"),
  (Pub ">" Nothing (Just []) Nothing, Left "explicit empty headers"),
  (Pub ">" Nothing (Just [("", "value")]) Nothing, Left "explicit empty header key"),
  (Pub ">" Nothing (Just [("key", "")]) Nothing, Left "explicit empty header value")
  ]

validateCases = parallel $ do
  describe "PUB validater" $ do
    forM_ explicitValidatorCases $ \(input, want) ->
      it (printf "correctly validates %s" (show input)) $ do
        validate input `shouldBe` want

