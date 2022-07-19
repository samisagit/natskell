{-# LANGUAGE OverloadedStrings #-}

module PingSpec (spec) where

import           Lib.Parser
import           Parsers.Parsers
import           Test.Hspec
import           Transformers.Transformers
import           Types.Ping

spec :: Spec
spec = do
  manual

manual :: Spec
manual = parallel $ do
  describe "parser" $ do
    it "correctly parses PING" $ do
      let result = fmap fst ping
      let rest = fmap snd ping
      case result of
        Just (ParsedPing a) -> a `shouldBe` Ping
        Nothing             -> error "parser did not return PING type"
      case rest of
        Just "" -> return ()
        _       -> error "parser did not consume all tokens"
  describe "transformer" $ do
    it "correctly transforms to 'PING'" $ do
      transform Ping `shouldBe` "PING\r\n"
  where
    ping = runParser pingParser "PING\r\n"
