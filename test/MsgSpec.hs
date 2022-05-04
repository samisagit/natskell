{-# LANGUAGE OverloadedStrings #-}

module MsgSpec (spec) where

import           Control.Monad
import           Data.ByteString
import           Msg
import           Parser
import           Test.Hspec
import           Text.Printf

cases :: [(ByteString, Data)]
cases = [
  ("MSG FOO 13 BAR 17\r\nsome payload bits\r\n", Data "FOO" 13 (Just "BAR") 17 (Just "some payload bits")),
  ("MSG FOO 13 BAR 0\r\n", Data "FOO" 13 (Just "BAR") 0 Nothing),
  ("MSG FOO 13 17\r\nsome payload bits\r\n", Data "FOO" 13 Nothing 17 (Just"some payload bits")),
  ("MSG FOO 13 0\r\n", Data "FOO" 13 Nothing 0 Nothing),
  ("MSG FOO 13 20\r\nmulti\r\nline\r\npayload\r\n", Data "FOO" 13 Nothing 20 (Just "multi\r\nline\r\npayload")),
  ("MSG FOO.BAR.BAZ 13 IN.*.BOX.> 0\r\n", Data "FOO.BAR.BAZ" 13 (Just "IN.*.BOX.>") 0 Nothing)
  ]

form = do
  describe "parser" $ do
    forM_ cases $ \(input, expected) ->
      it (printf "correctly parses %s" (show input)) $ do
        let output = runParser parser input
        let result = fmap fst output
        result `shouldBe` Just expected
        let left = fmap snd output
        left `shouldBe` Just ""

spec :: Spec
spec = do
  form

