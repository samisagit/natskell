{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
import           Data.Char
import qualified Data.Word8            as W8
import qualified Lib.Parser            as Parser
import           Test.Hspec
import           Text.Printf

spec :: Spec
spec = do
  char

charCases :: [(BS.ByteString, W8.Word8)]
charCases = zip (map charToByteString [(minBound::Char)..(maxBound::Char)]) [(minBound::W8.Word8)..(maxBound::W8.Word8)]
  where
    charToByteString c = B.pack [c]

char = parallel $ do
   forM_ charCases $ \(input, expected) ->
    describe (printf "char %s" (word8ToString expected)) $ do
      it (printf "correctly parses %s" (show input)) $ do
        let output = Parser.runParser (Parser.char expected) input
        let result = fmap fst output
        result `shouldBe` Just expected
        let left = fmap snd output
        left `shouldBe` Just ""
      it "returns Nothing given empty string" $ do
        let output = Parser.runParser (Parser.char expected) ""
        let result = fmap fst output
        result `shouldBe` Nothing
        let left = fmap snd output
        left `shouldBe` Nothing
      forM_ (filterSameChar input charCases) $ \(sinput, _) ->
        it (printf "returns Nothing given %s" (show sinput)) $ do
          let output = Parser.runParser (Parser.char expected) sinput
          let result = fmap fst output
          result `shouldBe` Nothing
          let left = fmap snd output
          left `shouldBe` Nothing

filterSameChar :: BS.ByteString -> [(BS.ByteString, W8.Word8)] -> [(BS.ByteString, W8.Word8)]
filterSameChar i [] = []
filterSameChar i os = filter f os
  where f item = fst item /= i

word8ToString :: W8.Word8 -> String
word8ToString w = show (BS.pack [w])
