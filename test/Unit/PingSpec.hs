{-# LANGUAGE OverloadedStrings #-}

module PingSpec (spec) where

import           Control.Monad
import qualified Data.ByteString           as BS
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Transformers.Transformers
import           Types.Ping

spec :: Spec
spec = do
  cases

explicitParserCases :: [(BS.ByteString, Ping)]
explicitParserCases = [("PING\r\n", Ping)]

explicitTransformerCases :: [(Ping, BS.ByteString)]
explicitTransformerCases = map (\(a,b) -> (b,a)) explicitParserCases

cases = parallel $ do
  describe "generic parser" $ do
    forM_ explicitParserCases $ \(input, want) -> do
      it (printf "correctly parses explicit case %s" (show input)) $ do
        let output = genericParse input
        output `shouldBe` Just (ParsedPing want)
  describe "PING transformer" $ do
    forM_ explicitTransformerCases $ \(input, want) -> do
      it (printf"correctly transforms %s" (show input)) $ do
        transform input `shouldBe` want
