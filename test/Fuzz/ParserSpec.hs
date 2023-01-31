module ParserSpec (spec) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
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

