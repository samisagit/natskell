{-# LANGUAGE OverloadedStrings #-}

module Client.Auth
  ( JwtBundle (..)
  , buildConnectPayload
  , defaultConnect
  , logAuthMethod
  , logTlsConfig
  , parseJwtBundle
  , signNonceWithSeed
  ) where

import           Client.RuntimeAPI
    ( Auth (..)
    , Config (..)
    , JWTTokenData
    , NKeyData
    , TLSCertData
    )
import qualified Crypto.Error          as Crypto
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.Bits             as Bits
import qualified Data.ByteArray        as ByteArray
import qualified Data.ByteString       as BS
import           Data.Char             (ord)
import qualified Data.List             as List
import           Data.Word             (Word16, Word32, Word8)
import           Lib.Logger            (AppM, LogLevel (..), MonadLogger (..))
import qualified Types.Connect         as Connect (Connect (..))

defaultConnect :: Connect.Connect
defaultConnect = Connect.Connect
  { Connect.verbose = False
  , Connect.pedantic = True
  , Connect.tls_required = False
  , Connect.auth_token = Nothing
  , Connect.user = Nothing
  , Connect.pass = Nothing
  , Connect.name = Nothing
  , Connect.lang = "haskell"
  , Connect.version = "0.1.0"
  , Connect.protocol = Nothing
  , Connect.echo = Just True
  , Connect.sig = Nothing
  , Connect.jwt = Nothing
  , Connect.nkey = Nothing
  , Connect.no_responders = Just True
  , Connect.headers = Just True
  }

logAuthMethod :: Auth -> AppM ()
logAuthMethod auth = case auth of
  None               -> logMessage Info "no authentication method provided"
  AuthToken _        -> logMessage Info "using auth token"
  UserPass (user, _) -> logMessage Info $ "using user/pass: " ++ show user
  NKey _             -> logMessage Info "using nkey"
  JWT _              -> logMessage Info "using jwt"

logTlsConfig :: Maybe TLSCertData -> AppM ()
logTlsConfig tlsConfig = case tlsConfig of
  Nothing -> pure ()
  Just _  -> logMessage Info "using tls certificate"

buildConnectPayload :: Config -> Maybe BS.ByteString -> Bool -> Connect.Connect
buildConnectPayload cfg nonce tlsRequired =
  applyAuthToConnect base (auth cfg) nonce
  where
    base = (connectConfig cfg) { Connect.tls_required = tlsRequired }

applyAuthToConnect :: Connect.Connect -> Auth -> Maybe BS.ByteString -> Connect.Connect
applyAuthToConnect base authType nonce =
  case authType of
    None ->
      base
    AuthToken token ->
      (clearAuthFields base) { Connect.auth_token = nonEmpty token }
    UserPass (user, pass) ->
      let (user', pass') = userPassFields user pass
      in (clearAuthFields base) { Connect.user = user', Connect.pass = pass' }
    NKey seed ->
      applyNKeyAuth base seed nonce
    JWT jwtInput ->
      applyJwtAuth base jwtInput nonce

clearAuthFields :: Connect.Connect -> Connect.Connect
clearAuthFields base =
  base
    { Connect.auth_token = Nothing
    , Connect.user = Nothing
    , Connect.pass = Nothing
    , Connect.sig = Nothing
    , Connect.jwt = Nothing
    , Connect.nkey = Nothing
    }

nonEmpty :: BS.ByteString -> Maybe BS.ByteString
nonEmpty value =
  if BS.null value
    then Nothing
    else Just value

userPassFields :: BS.ByteString -> BS.ByteString -> (Maybe BS.ByteString, Maybe BS.ByteString)
userPassFields user pass
  | BS.null user || BS.null pass = (Nothing, Nothing)
  | otherwise = (Just user, Just pass)

applyNKeyAuth :: Connect.Connect -> NKeyData -> Maybe BS.ByteString -> Connect.Connect
applyNKeyAuth base seed nonce =
  case nonce of
    Nothing -> clearAuthFields base
    Just nonceValue ->
      case signNonceWithSeed seed nonceValue of
        Left _ -> clearAuthFields base
        Right (publicKey, signature) ->
          (clearAuthFields base)
            { Connect.nkey = nonEmpty publicKey
            , Connect.sig = nonEmpty signature
            }

applyJwtAuth :: Connect.Connect -> JWTTokenData -> Maybe BS.ByteString -> Connect.Connect
applyJwtAuth base jwtInput nonce =
  case parseJwtBundle jwtInput of
    Nothing ->
      clearAuthFields base
    Just bundle ->
      let base' = (clearAuthFields base) { Connect.jwt = nonEmpty (jwtToken bundle) }
      in case nonce of
          Nothing ->
            clearAuthFields base
          Just nonceValue ->
            case signNonceWithSeed (jwtSeed bundle) nonceValue of
              Left _ -> clearAuthFields base
              Right (_, signature) ->
                base' { Connect.sig = nonEmpty signature }

data JwtBundle = JwtBundle
                   { jwtToken :: BS.ByteString
                   , jwtSeed  :: BS.ByteString
                   }
  deriving (Eq, Show)

parseJwtBundle :: BS.ByteString -> Maybe JwtBundle
parseJwtBundle input =
  fmap (uncurry JwtBundle) (parseCreds input)

signNonceWithSeed :: BS.ByteString -> BS.ByteString -> Either String (BS.ByteString, BS.ByteString)
signNonceWithSeed seed nonce = do
  seedBytes <- decodeSeed seed
  secretKey <- toSecretKey seedBytes
  let publicKey = Ed25519.toPublic secretKey
      signature = Ed25519.sign secretKey publicKey nonce
      sigEncoded = encodeBase64Url (ByteArray.convert signature :: BS.ByteString)
      publicEncoded = encodePublicKey (ByteArray.convert publicKey :: BS.ByteString)
  pure (publicEncoded, sigEncoded)

decodeSeed :: BS.ByteString -> Either String BS.ByteString
decodeSeed encoded = do
  raw <- decodeBase32 (trimAscii encoded)
  if BS.length raw < 4
    then Left "seed is too short"
    else do
      let (payload, checksumBytes) = BS.splitAt (BS.length raw - 2) raw
      expected <- decodeChecksum checksumBytes
      let actual = crc16 payload
      if actual /= expected
        then Left "seed checksum mismatch"
        else do
          let prefix0 = BS.index payload 0
              prefix1 = BS.index payload 1
              seedPrefix = prefix0 Bits..&. 0xF8
          if seedPrefix /= prefixByteSeed
            then Left "seed prefix mismatch"
            else do
              let typ = ((prefix0 Bits..&. 0x07) `Bits.shiftL` 5) Bits..|. (prefix1 `Bits.shiftR` 3)
              if typ /= prefixByteUser
                then Left "seed is not a user nkey"
                else do
                  let seedBytes = BS.drop 2 payload
                  if BS.length seedBytes /= 32
                    then Left "seed length is invalid"
                    else pure seedBytes

toSecretKey :: BS.ByteString -> Either String Ed25519.SecretKey
toSecretKey raw =
  case Ed25519.secretKey raw of
    Crypto.CryptoPassed key -> Right key
    Crypto.CryptoFailed _   -> Left "seed could not be parsed as a secret key"

parseCreds :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
parseCreds input = do
  jwt <- extractBlock jwtStart jwtEnd input
  seed <- extractBlock seedStart seedEnd input
  pure (jwt, seed)

extractBlock :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
extractBlock startMarker endMarker input = do
  let (_, rest) = BS.breakSubstring startMarker input
  if BS.null rest
    then Nothing
    else do
      let afterStart = BS.drop (BS.length startMarker) rest
          (block, _) = BS.breakSubstring endMarker afterStart
      if BS.null block
        then Nothing
        else Just (trimAscii block)

trimAscii :: BS.ByteString -> BS.ByteString
trimAscii =
  dropWhileEndAscii isSpaceAscii . BS.dropWhile isSpaceAscii

dropWhileEndAscii :: (Word8 -> Bool) -> BS.ByteString -> BS.ByteString
dropWhileEndAscii predicate =
  BS.reverse . BS.dropWhile predicate . BS.reverse

isSpaceAscii :: Word8 -> Bool
isSpaceAscii w =
  w == 9 || w == 10 || w == 13 || w == 32

encodePublicKey :: BS.ByteString -> BS.ByteString
encodePublicKey raw =
  let payload = BS.cons prefixByteUser raw
      checksum = crc16 payload
  in encodeBase32 (payload <> encodeChecksum checksum)

decodeBase32 :: BS.ByteString -> Either String BS.ByteString
decodeBase32 input =
  let cleaned = BS.map toUpperAscii input
  in go 0 0 [] (BS.unpack cleaned)
  where
    go :: Word32 -> Int -> [Word8] -> [Word8] -> Either String BS.ByteString
    go _ _ acc [] =
      Right (BS.pack (reverse acc))
    go buffer bits acc (x:xs) =
      case base32Value x of
        Nothing -> Left "invalid base32 character"
        Just value -> do
          let buffer' = (buffer `Bits.shiftL` 5) Bits..|. fromIntegral value
              bits' = bits + 5
          let (acc', buffer'', bits'') = flush buffer' bits' acc
          go buffer'' bits'' acc' xs
    flush :: Word32 -> Int -> [Word8] -> ([Word8], Word32, Int)
    flush buffer bits acc
      | bits >= 8 =
          let bits' = bits - 8
              byte = fromIntegral ((buffer `Bits.shiftR` bits') Bits..&. 0xFF)
          in flush buffer bits' (byte : acc)
      | otherwise =
          (acc, buffer, bits)

encodeBase32 :: BS.ByteString -> BS.ByteString
encodeBase32 input =
  let (acc, buffer, bits) = List.foldl' step ([], 0, 0) (BS.unpack input)
      acc' = if bits == 0 then acc else encodeRemaining acc buffer bits
  in BS.pack (reverse acc')
  where
    step :: ([Word8], Word32, Int) -> Word8 -> ([Word8], Word32, Int)
    step (out, buffer, bits) byte =
      let buffer' = (buffer `Bits.shiftL` 8) Bits..|. fromIntegral byte
          bits' = bits + 8
          (out', buffer'', bits'') = emit out buffer' bits'
      in (out', buffer'', bits'')
    emit :: [Word8] -> Word32 -> Int -> ([Word8], Word32, Int)
    emit out buffer bits
      | bits >= 5 =
          let bits' = bits - 5
              idx = fromIntegral ((buffer `Bits.shiftR` bits') Bits..&. 0x1F)
              char = base32Alphabet idx
          in emit (char : out) buffer bits'
      | otherwise =
          (out, buffer, bits)
    encodeRemaining :: [Word8] -> Word32 -> Int -> [Word8]
    encodeRemaining out buffer bits =
      let idx = fromIntegral ((buffer `Bits.shiftL` (5 - bits)) Bits..&. 0x1F)
      in base32Alphabet idx : out

base32Alphabet :: Word8 -> Word8
base32Alphabet idx =
  if idx < 26
    then fromIntegral (ord 'A' + fromIntegral idx)
    else fromIntegral (ord '2' + fromIntegral idx - 26)

base32Value :: Word8 -> Maybe Word8
base32Value w
  | w >= 65 && w <= 90 = Just (w - 65)
  | w >= 50 && w <= 55 = Just (w - 24)
  | otherwise          = Nothing

toUpperAscii :: Word8 -> Word8
toUpperAscii w
  | w >= 97 && w <= 122 = w - 32
  | otherwise = w

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

encodeBase64Url :: BS.ByteString -> BS.ByteString
encodeBase64Url input =
  let (out, buffer, bits) = List.foldl' step ([], 0 :: Word32, 0 :: Int) (BS.unpack input)
      out' = flush out buffer bits
  in BS.pack (reverse out')
  where
    step (acc, buffer, bits) byte =
      let buffer' = (buffer `Bits.shiftL` 8) Bits..|. fromIntegral byte
          bits' = bits + 8
          (acc', buffer'', bits'') = emit acc buffer' bits'
      in (acc', buffer'', bits'')
    emit acc buffer bits
      | bits >= 6 =
          let bits' = bits - 6
              idx = fromIntegral ((buffer `Bits.shiftR` bits') Bits..&. 0x3F)
              char = base64UrlAlphabet idx
          in emit (char : acc) buffer bits'
      | otherwise =
          (acc, buffer, bits)
    flush acc buffer bits
      | bits == 0 = acc
      | otherwise =
          let idx = fromIntegral ((buffer `Bits.shiftL` (6 - bits)) Bits..&. 0x3F)
          in base64UrlAlphabet idx : acc

base64UrlAlphabet :: Word8 -> Word8
base64UrlAlphabet idx
  | idx < 26 = fromIntegral (ord 'A' + fromIntegral idx)
  | idx < 52 = fromIntegral (ord 'a' + fromIntegral idx - 26)
  | idx < 62 = fromIntegral (ord '0' + fromIntegral idx - 52)
  | idx == 62 = 45
  | otherwise = 95

jwtStart, jwtEnd, seedStart, seedEnd :: BS.ByteString
jwtStart = "-----BEGIN NATS USER JWT-----"
jwtEnd = "------END NATS USER JWT------"
seedStart = "-----BEGIN USER NKEY SEED-----"
seedEnd = "------END USER NKEY SEED------"

prefixByteSeed, prefixByteUser :: Word8
prefixByteSeed = 18 `Bits.shiftL` 3
prefixByteUser = 20 `Bits.shiftL` 3
