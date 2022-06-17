module ParserSpec (spec) where

import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
import           Data.Char
import qualified Data.Word8            as W8
import qualified Parser
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
      it "passes quick check" $ property $
        \x -> propChar x
  describe "string" $ do
    modifyMaxSuccess (const 100000) $ do
      it "passes quick check" $ property $
        \x -> propString x

propChar :: Char -> String -> Bool
propChar c s = do
  let output = Parser.runParser (Parser.char cc) cbs
  if BS.length cbs > 0 && BS.head cbs == cc
    then fmap fst output == Just cc
  else fmap fst output == Nothing
  where
    charToByteString = \c -> B.pack [c]
    stringToByteString = \s -> B.pack s
    cc = BS.head $ charToByteString c
    cbs = stringToByteString s

propString :: String -> String -> Bool
propString p i = do
  let output = Parser.runParser (Parser.string cp) ci
  if (BS.take (length p) ci) == cp
    then fmap fst output == Just (BS.unpack cp)
  else fmap fst output == Nothing
  where
    stringToByteString = \s -> B.pack s
    cp = stringToByteString p
    ci = stringToByteString i

