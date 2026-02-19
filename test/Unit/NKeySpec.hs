{-# LANGUAGE OverloadedStrings #-}

module NKeySpec (spec) where

import qualified Auth.NKey       as NKey
import qualified Data.ByteString as BS
import           Test.Hspec

spec :: Spec
spec = do
  describe "parseJwtBundle" $ do
    it "extracts jwt and seed blocks" $ do
      let creds =
            "-----BEGIN NATS USER JWT-----\n\
            \jwt-token\n\
            \------END NATS USER JWT------\n\
            \-----BEGIN USER NKEY SEED-----\n\
            \seed-token\n\
            \------END USER NKEY SEED------"
      NKey.parseJwtBundle creds `shouldBe` Just (NKey.JwtBundle "jwt-token" "seed-token")

    it "returns Nothing when blocks are missing" $ do
      NKey.parseJwtBundle "missing blocks" `shouldBe` Nothing

  describe "signNonceWithSeed" $ do
    it "derives the expected public key from a seed" $ do
      let seed = "SUAHR6JNS2HKJQEAQFHYPOXFXWE4JXBPKUWFX3IMYU72UHOGXT3ZMVFHXI"
          expectedPub = "UAB7EFDOTOBBMPOCK4SXFA62FVZOADQDZOU2W4IUDCGFKJXYVOK3LV7X"
      case NKey.signNonceWithSeed seed "nonce-123" of
        Left err -> expectationFailure err
        Right (pub, sig) -> do
          pub `shouldBe` expectedPub
          BS.length sig `shouldBe` 86
