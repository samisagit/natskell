{-# LANGUAGE OverloadedStrings #-}

module NKeySpec where

import           Crypto.NKey
import qualified Data.ByteString as BS
import           Test.Hspec

spec :: Spec
spec = describe "NKey signing" $ do
  -- Test with a known test seed
  -- SUACSSL3UAHUDXKFSNVUZRF5UHPMWZ6BFDTJ7M6USDXIIEQHP5D6HCQHBU is a test seed
  -- This is NOT a production seed - it's for testing only

  it "rejects invalid base32 input" $ do
    let result = signNonce "not-valid-base32!!!" "testnonce"
    case result of
      Left (InvalidSeed _) -> return ()
      _                    -> expectationFailure "Expected InvalidSeed error"

  it "rejects seed with wrong length" $ do
    -- Valid base32 but wrong length (too short)
    let result = signNonce "AAAA" "testnonce"
    case result of
      Left (InvalidSeed _) -> return ()
      _                    -> expectationFailure "Expected InvalidSeed error"

  it "produces a base64 encoded signature" $ do
    -- Use a well-formed test seed (Seed + User type prefix, 32 bytes of zeros for seed)
    -- First 2 bytes encode seed type (S=18) and key type (U=20)
    -- Then 32 bytes of the actual seed
    -- Base32 encoding of [0x90, 0xa0] ++ replicate 32 0x00
    let testSeed = "SCQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    let result = signNonce testSeed "testnonce"
    case result of
      Right sig -> do
        -- Should be base64 encoded (64 byte signature -> 88 chars base64)
        BS.length sig `shouldBe` 88
      Left e -> expectationFailure $ "Signing failed: " ++ show e
