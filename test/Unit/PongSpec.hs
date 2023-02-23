{-# LANGUAGE OverloadedStrings #-}

module PongSpec (spec) where

import           Control.Monad
import qualified Data.ByteString           as BS
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Pong
import           Validators.Validators

spec :: Spec
spec = do
  cases
  validateCase

explicitParserCases :: [(BS.ByteString, Pong)]
explicitParserCases = [("PONG\r\n", Pong)]

explicitTransformerCases :: [(Pong, BS.ByteString)]
explicitTransformerCases = map (\(a,b) -> (b,a)) explicitParserCases

cases = parallel $ do
  describe "generic parser" $ do
    forM_ explicitParserCases $ \(input, want) -> do
      it (printf "correctly parses explicit case %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Right (ParsedPong want)
  describe "PONG transformer" $ do
    forM_ explicitTransformerCases $ \(input, want) -> do
      it (printf "correctly transforms %s" (show input)) $ do
        transform input `shouldBe` want

validateCase = parallel $ do
  describe "PONG validater" $ do
    it "correctly validates PONG" $ do
      validate Pong `shouldBe` Right ()

