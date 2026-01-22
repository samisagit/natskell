{-# LANGUAGE OverloadedStrings #-}

module Crypto.NKey
  ( signNonce
  , NKeyError(..)
  ) where

import qualified Crypto.Error           as CE
import qualified Crypto.PubKey.Ed25519  as Ed25519
import qualified Data.ByteArray         as BA
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base32 as B32
import qualified Data.ByteString.Base64 as B64

data NKeyError = InvalidSeed String
               | SigningFailed String
  deriving (Eq, Show)

-- | Sign a nonce with an NKey seed
-- NKey seeds are base32-encoded Ed25519 seeds with a prefix byte
-- The signature is returned as base64-encoded
signNonce :: BS.ByteString -> BS.ByteString -> Either NKeyError BS.ByteString
signNonce seed nonceBS = do
  -- Decode the base32 seed (NKey seeds are base32 encoded)
  rawSeed <- case B32.decodeBase32Unpadded seed of
    Left e  -> Left $ InvalidSeed (show e)
    Right s -> Right s

  -- NKey seeds have a 2-byte prefix (type byte + public key byte)
  -- The actual seed is 32 bytes after the prefix
  let seedBytes = BS.drop 2 rawSeed

  -- Ensure we have 32 bytes for Ed25519 seed
  if BS.length seedBytes /= 32
    then Left $ InvalidSeed $ "Expected 32 byte seed, got " ++ show (BS.length seedBytes)
    else do
      -- Create Ed25519 secret key from seed
      case Ed25519.secretKey seedBytes of
        CE.CryptoFailed e -> Left $ SigningFailed (show e)
        CE.CryptoPassed sk -> do
          let pk = Ed25519.toPublic sk
          -- Sign the nonce
          let sig = Ed25519.sign sk pk nonceBS
          -- Return base64 encoded signature
          Right $ B64.encode (BA.convert sig)
