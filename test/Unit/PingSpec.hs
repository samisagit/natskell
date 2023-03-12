{-# LANGUAGE OverloadedStrings #-}

module PingSpec (spec) where

import           Control.Monad
import qualified Data.ByteString           as BS
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Ping
import           Validators.Validators

spec :: Spec
spec = do
  parserCases
  transformerCases
  validateCase

explicitParserCases :: [(BS.ByteString, Ping)]
explicitParserCases = [("PING\r\n", Ping)]

explicitTransformerCases :: [(Ping, BS.ByteString)]
explicitTransformerCases = map (\(a,b) -> (b,a)) explicitParserCases

parserCases = parallel $ do
  describe "generic parser" $ do
    forM_ explicitParserCases $ \(input, want) -> do
      it (printf "correctly parses explicit case %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Right (ParsedPing want, "")

transformerCases = parallel $ do
  describe "PING transformer" $ do
    forM_ explicitTransformerCases $ \(input, want) -> do
      it (printf"correctly transforms %s" (show input)) $ do
        transform input `shouldBe` want

validateCase = parallel $ do
  describe "PING validater" $ do
    it "correctly validates PING" $ do
      validate Ping `shouldBe` Right ()
