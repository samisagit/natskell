{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Either
import qualified Lib.Parser            as Parser
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck

spec :: Spec
spec = do
  qc

qc = do
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
  describe "take" $ do
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check" . property $
        propTake

propChar :: Char -> String -> Bool
propChar c s = do
  let output = Parser.runParser (Parser.char parserExpectation) parserInput
  if BS.length parserInput > 0 && BS.head parserInput == parserExpectation
    then fmap fst output == Right parserExpectation
  else isLeft output
  where
    parserExpectation = BS.head $ charToByteString c
    parserInput = stringToByteString s

propString :: String -> String -> Bool
propString p i = do
  let output = Parser.runParser (Parser.string parserExpectation) parserInput
  if BS.take (length p) parserInput == parserExpectation
    then fmap fst output == Right (BS.unpack parserExpectation)
  else isLeft output
  where
    parserExpectation = stringToByteString p
    parserInput = stringToByteString i

propTil :: Char -> String -> Bool
propTil c i = do
  let output = Parser.runParser (Parser.til tilChar) parserInput
  case output of
    Right (struct, rest) -> tilChar `notElem` struct
    Left _               -> not (stringIsAscii i) || null i || c == head i || c `notElem` tail i
  where
    tilChar = BS.head $ charToByteString c
    parserInput = stringToByteString i

propTake :: Int -> String -> Bool
propTake n i = do
  let output = Parser.runParser (Parser.take' n Parser.ascii) parserInput
  case output of
    Right (struct, rest) -> length struct == n
    Left _               -> n < 0 || length i < n || not (stringIsAscii i)
  where
    parserInput = stringToByteString i

stringIsAscii :: String -> Bool
stringIsAscii = all isAscii

charToByteString c = B.pack [c]

stringToByteString = B.pack
