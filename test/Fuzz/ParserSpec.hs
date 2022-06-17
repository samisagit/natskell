module ParserSpec (spec) where

import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
import           Data.Char
import qualified Data.Word8            as W8
import           Data.Maybe
import qualified Lib.Parser            as Parser
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck
import           Text.Printf

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

propChar :: Char -> String -> Bool
propChar c s = do
  let output = Parser.runParser (Parser.char parserExpectation) parserInput
  if BS.length parserInput > 0 && BS.head parserInput == parserExpectation
    then fmap fst output == Just parserExpectation
  else isNothing output
  where
    charToByteString c = B.pack [c]
    stringToByteString = B.pack
    parserExpectation = BS.head $ charToByteString c
    parserInput = stringToByteString s

propString :: String -> String -> Bool
propString p i = do
  let output = Parser.runParser (Parser.string parserExpectation) parserInput
  if BS.take (length p) parserInput == parserExpectation
    then fmap fst output == Just (BS.unpack parserExpectation)
  else isNothing output
  where
    stringToByteString = B.pack
    parserExpectation = stringToByteString p
    parserInput = stringToByteString i

