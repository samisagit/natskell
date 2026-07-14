{-# LANGUAGE OverloadedStrings #-}

module NKeySpec (spec) where

import           Auth.Config           (mergeAuth)
import qualified Auth.Jwt              as Jwt
import qualified Auth.NKey             as NKey
import           Auth.Resolver         (buildAuthPatch)
import qualified Auth.Token            as Token
import           Auth.Types
    ( AuthContext (AuthContext)
    , AuthError (AuthError)
    , AuthPatch (..)
    , emptyAuthPatch
    )
import qualified Auth.UserPass         as UserPass
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Either           (isLeft)
import           Data.IORef
    ( atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
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
      Jwt.parseJwtBundle creds `shouldBe` Just (Jwt.JwtBundle "jwt-token" "seed-token")

    it "returns Nothing when blocks are missing" $ do
      Jwt.parseJwtBundle "missing blocks" `shouldBe` Nothing

    it "rejects a credentials bundle with a missing end marker" $ do
      let creds =
            "-----BEGIN NATS USER JWT-----\n\
            \jwt-token\n\
            \-----BEGIN USER NKEY SEED-----\n\
            \seed-token\n\
            \------END USER NKEY SEED------"
      Jwt.parseJwtBundle creds `shouldBe` Nothing

    it "rejects empty credential blocks" $ do
      let creds =
            "-----BEGIN NATS USER JWT-----\n\
            \------END NATS USER JWT------\n\
            \-----BEGIN USER NKEY SEED-----\n\
            \seed-token\n\
            \------END USER NKEY SEED------"
      Jwt.parseJwtBundle creds `shouldBe` Nothing

  describe "signNonceWithSeed" $ do
    it "derives the expected public key from a seed" $ do
      let seed = "SUAHR6JNS2HKJQEAQFHYPOXFXWE4JXBPKUWFX3IMYU72UHOGXT3ZMVFHXI"
          expectedPub = "UAB7EFDOTOBBMPOCK4SXFA62FVZOADQDZOU2W4IUDCGFKJXYVOK3LV7X"
      case NKey.signNonceWithSeed seed "nonce-123" of
        Left err -> expectationFailure err
        Right (pub, sig) -> do
          pub `shouldBe` expectedPub
          BS.length sig `shouldBe` 86

    it "rejects a seed with non-canonical trailing base32 data" $ do
      let seed = "SUAHR6JNS2HKJQEAQFHYPOXFXWE4JXBPKUWFX3IMYU72UHOGXT3ZMVFHXI"

      NKey.signNonceWithSeed (seed <> "A") "nonce-123" `shouldSatisfy` isLeft

    it "rejects lower-case seed encodings" $ do
      let seed = "suahr6jns2hkjqeaqfhypoxfxwe4jxbpkuwfx3imyu72uhogxt3zmvfhxi"

      NKey.signNonceWithSeed seed "nonce-123" `shouldSatisfy` isLeft

    it "rejects seeds with an invalid checksum" $ do
      let seed = "SUAHR6JNS2HKJQEAQFHYPOXFXWE4JXBPKUWFX3IMYU72UHOGXT3ZMVFHXJ"

      NKey.signNonceWithSeed seed "nonce-123" `shouldSatisfy` isLeft

  describe "secret rendering" $ do
    it "redacts credentials and authentication patches" $ do
      let bundle = Jwt.JwtBundle "jwt-secret" "seed-secret"
          patch = emptyAuthPatch
            { patchPass = Just "password-secret"
            , patchSig = Just "signature-secret"
            }

      show bundle `shouldNotContain` "jwt-secret"
      show bundle `shouldNotContain` "seed-secret"
      show patch `shouldNotContain` "password-secret"
      show patch `shouldNotContain` "signature-secret"

  describe "auth handlers" $ do
    it "fetches a fresh token for every connection attempt" $ do
      calls <- newIORef (0 :: Int)
      let handler = do
            call <- atomicModifyIORef' calls $ \current ->
              let next = current + 1
              in (next, next)
            pure (Right ("token-" <> BC.pack (show call)))
          auth = Token.authHandler handler

      first <- buildAuthPatch auth (AuthContext Nothing)
      second <- buildAuthPatch auth (AuthContext Nothing)

      fmap patchAuthToken first `shouldBe` Right (Just "token-1")
      fmap patchAuthToken second `shouldBe` Right (Just "token-2")

    it "allows token and user/pass credentials to coexist" $ do
      let auth = mergeAuth (Token.auth "token") (UserPass.auth ("user", "pass"))

      result <- buildAuthPatch auth (AuthContext Nothing)

      fmap patchAuthToken result `shouldBe` Right (Just "token")
      fmap patchUser result `shouldBe` Right (Just "user")
      fmap patchPass result `shouldBe` Right (Just "pass")

    it "base64url encodes raw signatures returned by handlers" $ do
      let publicKey = "UAB7EFDOTOBBMPOCK4SXFA62FVZOADQDZOU2W4IUDCGFKJXYVOK3LV7X"
          auth = NKey.authHandler publicKey $ \_ ->
            pure (Right (BS.replicate 64 0))

      result <- buildAuthPatch auth (AuthContext (Just "nonce"))

      fmap (fmap BS.length . patchSig) result `shouldBe` Right (Just 86)

    it "validates public nkeys before invoking signature handlers" $ do
      called <- newIORef False
      let auth = NKey.authHandler "USER_PUBLIC_KEY" $ \_ -> do
            writeIORef called True
            pure (Right (BS.replicate 64 0))

      result <- buildAuthPatch auth (AuthContext (Just "nonce"))

      result `shouldSatisfy` isLeft
      readIORef called `shouldReturn` False

    it "returns handler failures as typed authentication errors" $ do
      let auth = Token.authHandler (pure (Left (AuthError "token unavailable")))

      result <- buildAuthPatch auth (AuthContext Nothing)

      result `shouldBe` Left (AuthError "token unavailable")
