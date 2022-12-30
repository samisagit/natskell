{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.Word8            as W8
import qualified Lib.Parser            as Parser
import           Test.Hspec
import           Text.Printf

spec :: Spec
spec = do
  char
  headers

charCases :: [(BS.ByteString, W8.Word8)]
charCases = zip (map charToByteString [(minBound::Char)..(maxBound::Char)]) [(minBound::W8.Word8)..(maxBound::W8.Word8)]
  where
    charToByteString c = B.pack [c]

char = parallel $ do
  describe "generated" $ do
    forM_ charCases $ \(input, want) ->
     describe (printf "char %s" (word8ToString want)) $ do
       it (printf "correctly parses generated case %s" (show input)) $ do
         let output = Parser.runParser (Parser.char want) input
         let result = fmap fst output
         result `shouldBe` Just want
         let left = fmap snd output
         left `shouldBe` Just ""
       it "returns Nothing given empty string" $ do
         let output = Parser.runParser (Parser.char want) ""
         let result = fmap fst output
         result `shouldBe` Nothing
         let left = fmap snd output
         left `shouldBe` Nothing
       forM_ (filterSameChar input charCases) $ \(sinput, _) ->
         it (printf "returns Nothing given %s" (show sinput)) $ do
           let output = Parser.runParser (Parser.char want) sinput
           let result = fmap fst output
           result `shouldBe` Nothing
           let left = fmap snd output
           left `shouldBe` Nothing

headerCases :: [(BS.ByteString, [(BS.ByteString, BS.ByteString)])]
headerCases = [
  ("NATS/1.0\r\nKEY: VALUE\r\n", [("KEY", "VALUE")]),
  ("NATS/1.0\r\nKEY:VALUE\r\n", [("KEY", "VALUE")]),
  ("NATS/1.0\r\n  KEY  : VALUE  \r\n", [("KEY", "VALUE")]),
  ("NATS/1.0\r\nKEY: VALUE\r\nOTHER: STUFF\r\n", [("KEY", "VALUE"), ("OTHER", "STUFF")]),
  ("NATS/1.0\r\n", []),
  ("NATS/2.2\r\n", [])
  ]

headers = parallel $ do
  describe "headers" $ do
    forM_ headerCases $ \(input, want) ->
      it (printf "correctly parses explicit case %s" (show input)) $ do
        let output = Parser.runParser (Parser.headersParser (fromIntegral . BS.length $ input)) input
        let result = fmap fst output
        result `shouldBe` Just want

filterSameChar :: BS.ByteString -> [(BS.ByteString, W8.Word8)] -> [(BS.ByteString, W8.Word8)]
filterSameChar i [] = []
filterSameChar i os = filter f os
  where f item = fst item /= i

word8ToString :: W8.Word8 -> String
word8ToString w = show (BS.pack [w])

