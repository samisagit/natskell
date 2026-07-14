module Auth.NKey.Codec
  ( encodeSignature
  , signNonceWithSeedRaw
  , validateUserPublicKey
  ) where

import qualified Crypto.Error            as Crypto
import qualified Crypto.PubKey.Ed25519   as Ed25519
import qualified Data.Bits               as Bits
import qualified Data.ByteArray          as ByteArray
import qualified Data.ByteArray.Encoding as Encoding
import qualified Data.ByteString         as BS
import qualified Data.List               as List
import           Data.Word               (Word16, Word8)

signNonceWithSeedRaw
  :: BS.ByteString
  -> BS.ByteString
  -> Either String (BS.ByteString, BS.ByteString)
signNonceWithSeedRaw seed nonce = do
  seedBytes <- decodeSeed seed
  secretKey <- toSecretKey seedBytes
  let publicKey = Ed25519.toPublic secretKey
      signature = Ed25519.sign secretKey publicKey nonce
      signatureBytes = ByteArray.convert signature
      publicBytes = ByteArray.convert publicKey
  pure (encodePublicKey publicBytes, signatureBytes)

encodeSignature :: BS.ByteString -> BS.ByteString
encodeSignature = Encoding.convertToBase Encoding.Base64URLUnpadded

validateUserPublicKey :: BS.ByteString -> Either String ()
validateUserPublicKey encoded = do
  raw <- decodeBase32Canonical encoded
  payload <- verifyChecksum "public key" raw
  if BS.length payload /= 33
    then Left "public key length is invalid"
    else
      if BS.head payload /= prefixByteUser
        then Left "public key is not a user nkey"
        else Right ()

decodeSeed :: BS.ByteString -> Either String BS.ByteString
decodeSeed encoded = do
  raw <- decodeBase32Canonical (trimAscii encoded)
  payload <- verifyChecksum "seed" raw
  if BS.length payload /= 34
    then Left "seed length is invalid"
    else do
      let prefix0 = BS.index payload 0
          prefix1 = BS.index payload 1
          seedPrefix = prefix0 Bits..&. 0xF8
          publicPrefix =
            ((prefix0 Bits..&. 0x07) `Bits.shiftL` 5)
              Bits..|. (prefix1 `Bits.shiftR` 3)
      if seedPrefix /= prefixByteSeed
        then Left "seed prefix mismatch"
        else
          if publicPrefix /= prefixByteUser
            then Left "seed is not a user nkey"
            else Right (BS.drop 2 payload)

decodeBase32Canonical :: BS.ByteString -> Either String BS.ByteString
decodeBase32Canonical encoded = do
  let paddingLength = (8 - BS.length encoded `mod` 8) `mod` 8
      padded = encoded <> BS.replicate paddingLength 61
  decoded <-
    case Encoding.convertFromBase Encoding.Base32 padded of
      Left err    -> Left ("invalid base32 encoding: " ++ err)
      Right bytes -> Right bytes
  let canonical = BS.takeWhile (/= 61) (Encoding.convertToBase Encoding.Base32 decoded)
  if canonical == encoded
    then Right decoded
    else Left "base32 encoding is not canonical"

verifyChecksum :: String -> BS.ByteString -> Either String BS.ByteString
verifyChecksum label raw
  | BS.length raw < 3 = Left (label ++ " is too short")
  | otherwise = do
      let (payload, checksumBytes) = BS.splitAt (BS.length raw - 2) raw
      expected <- decodeChecksum checksumBytes
      if crc16 payload == expected
        then Right payload
        else Left (label ++ " checksum mismatch")

toSecretKey :: BS.ByteString -> Either String Ed25519.SecretKey
toSecretKey raw =
  case Ed25519.secretKey raw of
    Crypto.CryptoPassed key -> Right key
    Crypto.CryptoFailed _   -> Left "seed could not be parsed as a secret key"

encodePublicKey :: BS.ByteString -> BS.ByteString
encodePublicKey raw =
  let payload = BS.cons prefixByteUser raw
      checksum = crc16 payload
  in Encoding.convertToBase Encoding.Base32 (payload <> encodeChecksum checksum)

encodeChecksum :: Word16 -> BS.ByteString
encodeChecksum value =
  BS.pack
    [ fromIntegral (value Bits..&. 0xFF)
    , fromIntegral (value `Bits.shiftR` 8)
    ]

decodeChecksum :: BS.ByteString -> Either String Word16
decodeChecksum bytes
  | BS.length bytes /= 2 = Left "checksum length is invalid"
  | otherwise =
      let b0 = fromIntegral (BS.index bytes 0)
          b1 = fromIntegral (BS.index bytes 1)
      in Right (b0 Bits..|. (b1 `Bits.shiftL` 8))

crc16 :: BS.ByteString -> Word16
crc16 =
  List.foldl' update 0 . BS.unpack
  where
    update crc byte =
      List.foldl' step (crc `Bits.xor` (fromIntegral byte `Bits.shiftL` 8)) [0 .. 7]

    step crc _ =
      if Bits.testBit crc 15
        then (crc `Bits.shiftL` 1) `Bits.xor` 0x1021
        else crc `Bits.shiftL` 1

trimAscii :: BS.ByteString -> BS.ByteString
trimAscii =
  dropWhileEndAscii isSpaceAscii . BS.dropWhile isSpaceAscii

dropWhileEndAscii :: (Word8 -> Bool) -> BS.ByteString -> BS.ByteString
dropWhileEndAscii predicate =
  BS.reverse . BS.dropWhile predicate . BS.reverse

isSpaceAscii :: Word8 -> Bool
isSpaceAscii w =
  w == 9 || w == 10 || w == 13 || w == 32

prefixByteSeed, prefixByteUser :: Word8
prefixByteSeed = 18 `Bits.shiftL` 3
prefixByteUser = 20 `Bits.shiftL` 3
