{-# LANGUAGE OverloadedStrings #-}

module OkSpec (spec) where

import           Control.Monad
import qualified Data.ByteString as BS
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Types.Ok

spec :: Spec
spec = do
  cases

explicitCases :: [(BS.ByteString, Ok)]
explicitCases = [("+OK\r\n", Ok)]

cases = parallel $ do
  describe "generic parser" $ do
      forM_ explicitCases $ \(input, want) ->
        it (printf "correctly parses explicit case %s" (show input)) $ do
          let output = genericParse input
          output `shouldBe` Just (ParsedOk want)
