{-# LANGUAGE OverloadedStrings #-}

module NuidSpec (spec) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Nuid
import           System.Random         (mkStdGen)
import           Test.Hspec

spec :: Spec
spec = describe "NUID generation" $ do
  it "returns base62 tokens with the expected length" $ do
    let nuid0 = newNuid (mkStdGen 1)
        (token, _) = nextNuid nuid0
    BS.length token `shouldBe` 22
    BC.all (`BC.elem` base62Alphabet) token `shouldBe` True

base62Alphabet :: BC.ByteString
base62Alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
