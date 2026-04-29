{-# LANGUAGE OverloadedStrings #-}

module OkSpec (spec) where

import           Control.Monad
import qualified Data.ByteString as BS
import           Parser.API
    ( ParseStep (Emit)
    , ParsedMessage (ParsedOk)
    , parse
    )
import           Parser.Nats     (parserApi)
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
          let output = parse parserApi input
          output `shouldBe` Emit (ParsedOk want) ""
