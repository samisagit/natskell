{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.Word8            as W8
import qualified Lib.Parser            as Parser
import qualified Parsers.Parsers       as P
import           Test.Hspec
import           Text.Printf

spec :: Spec
spec = do
  char
  headers
  depth

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
         result `shouldBe` Right want
         let left = fmap snd output
         left `shouldBe` Right ""
       it "returns ParserErr given empty string" $ do
         let output = Parser.runParser (Parser.char want) ""
         let result = fmap fst output
         result `shouldBe` Left (Parser.UnexpectedEndOfInput "nothing to read" 0)
         let left = fmap snd output
         left `shouldBe` Left (Parser.UnexpectedEndOfInput "nothing to read" 0)
       forM_ (filterSameChar input charCases) $ \(sinput, _) ->
         it (printf "returns ParserErr given %s" (show sinput)) $ do
           let output = Parser.runParser (Parser.char want) sinput
           let result = fmap fst output
           result `shouldBe` Left (Parser.UnexpectedChar(B.unpack $ foldr BS.append "" [BS.pack [BS.head sinput],  " does not match ", BS.pack [want], " in ", sinput]) 1)
           let left = fmap snd output
           left `shouldBe` Left (Parser.UnexpectedChar(B.unpack $ foldr BS.append "" [BS.pack [BS.head sinput],  " does not match ", BS.pack [want], " in ", sinput]) 1)

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
        result `shouldBe` Right want

depth = parallel $ do
  describe "alternative parser" $ do
    it "returns deepest failed parse attempt error" $ do
      let outputA = Parser.runParser (P.pingParser <|> P.pongParser) "PIL"
      let outputB = Parser.runParser (P.pongParser <|> P.pingParser) "PIL"
      outputA `shouldBe` outputB
      outputA `shouldBe` Left (Parser.UnexpectedChar "L does not match N in L" 1)

filterSameChar :: BS.ByteString -> [(BS.ByteString, W8.Word8)] -> [(BS.ByteString, W8.Word8)]
filterSameChar _ [] = []
filterSameChar i os = filter f os
  where f item = fst item /= i

word8ToString :: W8.Word8 -> String
word8ToString w = show (BS.pack [w])

