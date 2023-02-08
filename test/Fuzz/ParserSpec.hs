{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Either
import qualified Data.Word8            as W8
import qualified Lib.Parser            as Parser
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck

spec :: Spec
spec = do
  qc

qc = do
  describe "til" $ do
    it "qq" $ do
      let output = Parser.runParser (Parser.til W8._greater) "cheat code for greater than\59966"
      output `shouldBe` Right (BS.unpack "cheat code for greater than", ">")
  describe "char" $ do
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check" . property $
        propChar
  describe "string" $ do
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check" . property $
        propString
  describe "til" $ do
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check" . property $
        propTil
  -- test take

propChar :: Char -> String -> Bool
propChar c s = do
  let output = Parser.runParser (Parser.char parserExpectation) parserInput
  if BS.length parserInput > 0 && BS.head parserInput == parserExpectation
    then fmap fst output == Right parserExpectation
  else isLeft output
  where
    charToByteString c = B.pack [c]
    stringToByteString = B.pack
    parserExpectation = BS.head $ charToByteString c
    parserInput = stringToByteString s

propString :: String -> String -> Bool
propString p i = do
  let output = Parser.runParser (Parser.string parserExpectation) parserInput
  if BS.take (length p) parserInput == parserExpectation
    then fmap fst output == Right (BS.unpack parserExpectation)
  else isLeft output
  where
    stringToByteString = B.pack
    parserExpectation = stringToByteString p
    parserInput = stringToByteString i

propTil :: Char -> String -> Bool
propTil c i = do
  let output = Parser.runParser (Parser.til tilChar) parserInput

  case output of
    Right (struct, rest) -> tilChar `notElem` struct
    Left _               -> not (stringIsAscii i) || null i || c == head i || c `notElem` tail i
  where
    charToByteString c = B.pack [c]
    stringToByteString = B.pack
    tilChar = BS.head $ charToByteString c
    parserInput = stringToByteString i

stringIsAscii :: String -> Bool
stringIsAscii = all isAscii

