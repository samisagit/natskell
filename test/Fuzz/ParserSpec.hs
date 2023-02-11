{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Either
import           Data.Maybe
import qualified Lib.Parser            as Parser
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck
import           Text.Read

spec :: Spec
spec = do
  qc

qc = do
  describe "char" $ do
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check" . property $
        propChar
  describe "charIn" $ do
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check" . property $
        propCharIn
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
      it "passes quick check for ascii" . property $
        propTakeAscii
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check for digit" . property $
        propTakeDigit

propChar :: Char -> String -> Bool
propChar c s = do
  let output = Parser.runParser (Parser.char parserExpectation) parserInput
  if BS.length parserInput > 0 && BS.head parserInput == parserExpectation
    then fmap fst output == Right parserExpectation
  else isLeft output
  where
    parserExpectation = BS.head $ charToByteString c
    parserInput = stringToByteString s

propCharIn :: [Char] -> String -> Bool
propCharIn cs s = do
  let output = Parser.runParser (Parser.charIn parserExpectation) parserInput
  case output of
    Right (struct, rest) -> BS.head parserInput `BS.elem` BS.pack parserExpectation
    Left _               -> BS.null parserInput || BS.head parserInput `BS.notElem` BS.pack parserExpectation
  where
    parserExpectation = map charToW8 cs
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

-- TODO: try removing ascii filter, and making conditions based on the bytestring
-- inputs and expectations.

propTil :: Char -> String -> Bool
propTil c i = do
  let output = Parser.runParser (Parser.til tilChar) parserInput
  case output of
    Right (struct, rest) -> tilChar `notElem` struct
    Left _               -> BS.null parserInput || tilChar == BS.head parserInput || tilChar `BS.notElem` BS.tail parserInput
  where
    tilChar = BS.head $ charToByteString c
    parserInput = stringToByteString i

propTakeAscii :: Int -> String -> Bool
propTakeAscii n i = do
  let output = Parser.runParser (Parser.take' n Parser.ascii) parserInput
  case output of
    Right (struct, rest) -> length struct == n
    Left _               -> n < 0 || length i < n || not (stringIsAscii i)
  where
    parserInput = stringToByteString i

propTakeDigit :: Int -> String -> Bool
propTakeDigit n i = do
  let output = Parser.runParser (Parser.take' n Parser.digit) parserInput
  case output of
    Right (struct, rest) -> length struct == n
    Left _               -> n < 0 || length i < n || not (stringIsNum i)
  where
    parserInput = stringToByteString i

stringIsAscii :: String -> Bool
stringIsAscii = all isAscii

charToByteString c = B.pack [c]

stringToByteString = B.pack

charToW8 = BS.head . charToByteString

stringIsNum :: String -> Bool
stringIsNum = all ((== True) . charIsDigit)

charIsDigit :: Char -> Bool
charIsDigit = isJust . maybeIntId . readMaybe . (:[])

maybeIntId :: Maybe Int -> Maybe Int
maybeIntId = id

