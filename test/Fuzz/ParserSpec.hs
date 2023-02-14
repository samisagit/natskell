{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
import           Data.Char
import           Data.Either
import           Data.Maybe
import qualified Data.Word8            as W8
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
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check for space" . property $
        propSpace
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check for ss" . property $
        propSomeSpace
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check for stringWithChars" . property $
        propStringWithChars
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check for alphaNumeric" . property $
        propAlphaNumeric

propChar :: W8.Word8 -> BS.ByteString -> Bool
propChar c s = do
  let output = Parser.runParser (Parser.char c) s
  if BS.length s > 0 && BS.head s == c
    then fmap fst output == Right c
  else isLeft output

propCharIn :: [W8.Word8] -> BS.ByteString -> Bool
propCharIn cs s = do
  let output = Parser.runParser (Parser.charIn cs) s
  case output of
    Right (struct, rest) -> BS.head s `BS.elem` BS.pack cs
    Left _               -> BS.null s || BS.head s `BS.notElem` BS.pack cs

propString :: BS.ByteString -> BS.ByteString -> Bool
propString p i = do
  let output = Parser.runParser (Parser.string p) i
  if BS.take (BS.length p) i == p
    then fmap fst output == Right (BS.unpack p)
  else isLeft output

propTil :: W8.Word8 -> BS.ByteString -> Bool
propTil c i = do
  let output = Parser.runParser (Parser.til c) i
  case output of
    Right (struct, "")   -> False -- til should always leave `c` char in rest (#93)
    Right (struct, rest) -> c `notElem` struct && BS.head rest == c
    Left _               -> BS.null i || c == BS.head i || c `BS.notElem` BS.tail i

propTakeAscii :: Int -> BS.ByteString -> Bool
propTakeAscii n i = do
  let output = Parser.runParser (Parser.take' n Parser.ascii) i
  case output of
    Right (struct, rest) -> length struct == n
    Left _               -> n < 0 || BS.length i < n || not (stringIsAscii i)

propTakeDigit :: Int -> BS.ByteString -> Bool
propTakeDigit n i = do
  let output = Parser.runParser (Parser.take' n Parser.digit) i
  case output of
    Right (struct, rest) -> length struct == n
    Left _               -> n < 0 || BS.length i < n || not (stringIsNum i)

propSpace :: BS.ByteString -> Bool
propSpace i = do
  let output = Parser.runParser Parser.space i
  case output of
    Right (struct, rest) -> BS.length rest == BS.length i -1 && struct == W8._space
    Left _               -> BS.null i || BS.head i /= W8._space

propSomeSpace :: BS.ByteString -> Bool
propSomeSpace i = do
  let output = Parser.runParser Parser.ss i
  case output of
    Right (struct, rest) -> all (== W8._space) struct && (BS.null rest || BS.head rest /= W8._space)
    Left _               -> BS.null i || BS.head i /= W8._space

propStringWithChars :: [W8.Word8] -> BS.ByteString -> Bool
propStringWithChars cs i = do
  let output = Parser.runParser (Parser.stringWithChars cs) i
  case output of
    Right (struct, rest) -> all (`BS.elem` BS.pack cs) struct && (BS.null rest || not (BS.elem (BS.head rest) (BS.pack cs)))
    Left _               -> BS.null i || not (BS.elem (BS.head i) (BS.pack cs))

propAlphaNumeric :: BS.ByteString -> Bool
propAlphaNumeric i = do
  let output = Parser.runParser Parser.alphaNumeric i
  case output of
    Right (struct, rest) -> BS.length rest == BS.length i -1 && W8.isAlphaNum struct
    Left _               -> BS.null i || not (W8.isAlphaNum (BS.head i))

stringIsAscii :: BS.ByteString -> Bool
stringIsAscii = all isAscii . B.unpack

stringIsNum :: BS.ByteString -> Bool
stringIsNum = all ((== True) . charIsDigit) . B.unpack

charIsDigit :: Char -> Bool
charIsDigit = isJust . maybeIntId . readMaybe . (:[])

maybeIntId :: Maybe Int -> Maybe Int
maybeIntId = id

instance Arbitrary BS.ByteString where arbitrary = BS.pack <$> arbitrary
