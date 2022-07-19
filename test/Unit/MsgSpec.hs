{-# LANGUAGE OverloadedStrings #-}

module MsgSpec (spec) where

import           Control.Monad
import           Data.ByteString
import           Lib.Parser
import           Parsers.Parsers
import           Test.Hspec
import           Text.Printf
import           Types.Msg

cases :: [(ByteString, Msg)]
cases = [
  ("MSG FOO 13 BAR 17\r\nsome payload bits\r\n", Msg "FOO" 13 (Just "BAR") 17 (Just "some payload bits")),
  ("MSG FOO 13 BAR 0\r\n", Msg "FOO" 13 (Just "BAR") 0 Nothing),
  ("MSG FOO 13 17\r\nsome payload bits\r\n", Msg "FOO" 13 Nothing 17 (Just"some payload bits")),
  ("MSG FOO 13 0\r\n", Msg "FOO" 13 Nothing 0 Nothing),
  ("MSG FOO 13 20\r\nmulti\r\nline\r\npayload\r\n", Msg "FOO" 13 Nothing 20 (Just "multi\r\nline\r\npayload")),
  ("MSG FOO.BAR.BAZ 13 IN.*.BOX.> 0\r\n", Msg "FOO.BAR.BAZ" 13 (Just "IN.*.BOX.>") 0 Nothing)
  ]

form = parallel $ do
  describe "parser" $ do
    forM_ cases $ \(input, expected) ->
      it (printf "correctly parses %s" (show input)) $ do
        let output = runParser msgParser input
        let result = fmap fst output
        let rest = fmap snd output
        case result of
          Just (ParsedMsg a) -> a `shouldBe` expected
          Nothing            -> error "parser did not return MSG type"
        case rest of
          Just "" -> return ()
          _       -> error "parser did not consume all tokens"

spec :: Spec
spec = do
  form

