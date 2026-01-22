# JWT + NKey Authentication Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enable combined JWT + NKey authentication where the client sends both a JWT token and a signature of the server's nonce signed with the NKey seed.

**Architecture:** The server sends a nonce in INFO. The client stores this nonce, then when connecting, signs it with the NKey seed using Ed25519 and sends both the JWT and signature in CONNECT. This requires adding the `nonce` field to `Info`, a new `JWTWithNKey` auth variant, and Ed25519 signing logic.

**Tech Stack:** Haskell, Ed25519 (cryptonite), base32 (for NKey seed decoding), base64-bytestring (for signature encoding)

---

## Task 1: Add nonce field to Info type

**Files:**
- Modify: `internal/Types/Info.hs:12-27`
- Test: `test/Unit/InfoSpec.hs`

**Step 1: Write the failing test**

In `test/Unit/InfoSpec.hs`, add a test for parsing INFO with nonce:

```haskell
-- Add to imports if not present
import qualified Data.ByteString as BS

-- Add new test case in the spec
    it "parses info with nonce" $ do
      let json = "{\"server_id\":\"test\",\"version\":\"2.0\",\"go\":\"go1.19\",\"host\":\"0.0.0.0\",\"port\":4222,\"max_payload\":1048576,\"proto\":1,\"nonce\":\"abc123nonce\"}"
      let result = eitherDecode (BSL.fromStrict json) :: Either String Info
      case result of
        Right info -> nonce info `shouldBe` Just "abc123nonce"
        Left e -> expectationFailure e
```

**Step 2: Run test to verify it fails**

Run: `cabal test natskell:test:unit-test --test-options="-m 'parses info with nonce'"`
Expected: FAIL - `nonce` field not found in Info type

**Step 3: Add nonce field to Info type**

In `internal/Types/Info.hs`, add the nonce field:

```haskell
data Info = Info
              { server_id     :: BS.ByteString
              , version       :: BS.ByteString
              , go            :: BS.ByteString
              , host          :: BS.ByteString
              , port          :: Int
              , max_payload   :: Int
              , proto         :: Int
              , client_id     :: Maybe Int
              , auth_required :: Maybe Bool
              , tls_required  :: Maybe Bool
              , connect_urls  :: Maybe [BS.ByteString]
              , ldm           :: Maybe Bool
              , headers       :: Maybe Bool
              , nonce         :: Maybe BS.ByteString
              }
  deriving (Eq, Generic, Show)
```

**Step 4: Run test to verify it passes**

Run: `cabal test natskell:test:unit-test --test-options="-m 'parses info with nonce'"`
Expected: PASS

**Step 5: Commit**

```bash
git add internal/Types/Info.hs test/Unit/InfoSpec.hs
git commit -m "$(cat <<'EOF'
feat(auth): add nonce field to Info type

The NATS server sends a nonce in the INFO message for challenge-response
authentication. This field is required for NKey signature generation.
EOF
)"
```

---

## Task 2: Add JWTWithNKey auth variant

**Files:**
- Modify: `client/Options.hs:25-31`
- Modify: `client/Client.hs:304-311` (logAuthMethod)

**Step 1: Add JWTWithNKey to Auth type**

In `client/Options.hs`, update the Auth type:

```haskell
data Auth = None
          | UserPass UserPassData
          | NKey NKeyData
          | AuthToken AuthTokenData
          | JWT JWTTokenData
          | JWTWithNKey JWTTokenData NKeyData
          | TLSCert TLSCertData
  deriving (Eq, Show)
```

**Step 2: Add withJWTCreds helper function**

In `client/Options.hs`, add after line 55 (after withJWT):

```haskell
withJWTCreds :: JWTTokenData -> NKeyData -> ConfigOpts
withJWTCreds jwt nkey config = config { auth = JWTWithNKey jwt nkey }
```

**Step 3: Update logAuthMethod in Client.hs**

In `client/Client.hs`, update the logAuthMethod function:

```haskell
logAuthMethod :: Auth -> AppM ()
logAuthMethod auth = case auth of
  None               -> logInfo "No authentication method provided"
  AuthToken _        -> logInfo "Using auth token"
  UserPass (user, _) -> logInfo $ "Using user/pass: " ++ show user
  NKey _             -> logInfo "Using NKey"
  JWT _              -> logInfo "Using JWT"
  JWTWithNKey _ _    -> logInfo "Using JWT with NKey signature"
  TLSCert _          -> logInfo "Using TLS certificate"
```

**Step 4: Export withJWTCreds from Client module**

In `client/Client.hs`, add to the export list (around line 23):

```haskell
  withJWTCreds,
```

**Step 5: Build to verify compilation**

Run: `cabal build`
Expected: Build succeeds

**Step 6: Commit**

```bash
git add client/Options.hs client/Client.hs
git commit -m "$(cat <<'EOF'
feat(auth): add JWTWithNKey auth variant

Adds a combined auth type that holds both the JWT token and NKey seed.
This is needed for NATS JWT auth which requires both the JWT and a
signature of the server nonce.
EOF
)"
```

---

## Task 3: Add cryptographic dependencies

**Files:**
- Modify: `natskell.cabal:38-49`

**Step 1: Add dependencies to main library**

In `natskell.cabal`, add to the library build-depends (around line 49):

```cabal
library
  import:         shared
  build-depends:
    natskell-internal,
    -- TODO: check these are the highest possible values
    bytestring >= 0.10,
    containers < 0.7,
    hashable  < 1.5,
    stm  < 2.6,
    text  >= 1.2 && < 2.2,
    network  < 3.3,
    bytestring  >= 0.10 && < 0.13,
    network-simple  < 0.5,
    random  < 1.3,
    cryptonite >= 0.30,
    memory >= 0.18,
    base64-bytestring >= 1.2,
    base32 >= 0.4
```

**Step 2: Build to verify dependencies resolve**

Run: `cabal build`
Expected: Build succeeds (dependencies download and resolve)

**Step 3: Commit**

```bash
git add natskell.cabal
git commit -m "$(cat <<'EOF'
build: add cryptographic dependencies for NKey signing

Adds cryptonite for Ed25519, memory for secure byte handling,
base64-bytestring for encoding signatures, and base32 for decoding
NKey seeds.
EOF
)"
```

---

## Task 4: Create NKey signing module

**Files:**
- Create: `client/Crypto/NKey.hs`
- Modify: `natskell.cabal:54-60` (other-modules)

**Step 1: Create the NKey module**

Create `client/Crypto/NKey.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Crypto.NKey
  ( signNonce
  , NKeyError(..)
  ) where

import qualified Crypto.Error           as CE
import qualified Crypto.PubKey.Ed25519  as Ed25519
import qualified Data.Base32            as B32
import qualified Data.Base64.Types      as B64T
import qualified Data.ByteArray         as BA
import qualified Data.ByteString        as BS
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
          Right $ B64.encodeBase64' (BA.convert sig)
```

**Step 2: Add module to cabal file**

In `natskell.cabal`, update other-modules in the library section:

```cabal
  other-modules:
    Types
    Sid
    Options
    MSGView
    State
    Crypto.NKey
```

**Step 3: Create the directory structure**

Run: `mkdir -p client/Crypto`

**Step 4: Build to verify compilation**

Run: `cabal build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add client/Crypto/NKey.hs natskell.cabal
git commit -m "$(cat <<'EOF'
feat(auth): add NKey signing module

Implements Ed25519 signature generation for NATS NKey authentication.
Decodes base32 NKey seeds and produces base64-encoded signatures.
EOF
)"
```

---

## Task 5: Add unit tests for NKey signing

**Files:**
- Create: `test/Unit/NKeySpec.hs`
- Modify: `natskell.cabal:143-156` (unit-test other-modules)

**Step 1: Create NKey test module**

Create `test/Unit/NKeySpec.hs`:

```haskell
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
    -- Use a well-formed test seed (type U = user, 32 bytes of zeros for seed)
    -- Type byte: 'S' (0x53 = seed), then 'U' (0x55 = user type)
    -- Then 32 bytes of the actual seed
    -- Base32 encoding of this pattern
    let testSeed = "SUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    let result = signNonce testSeed "testnonce"
    case result of
      Right sig -> do
        -- Should be base64 encoded (64 byte signature -> ~88 chars base64)
        BS.length sig `shouldSatisfy` (> 0)
      Left e -> expectationFailure $ "Signing failed: " ++ show e
```

**Step 2: Add to cabal file**

In `natskell.cabal`, add `NKeySpec` to unit-test other-modules and add dependencies:

```cabal
test-suite unit-test
  import:         shared
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test/Unit
  main-is:          Spec.hs
  build-depends:
    natskell,
    natskell-internal,
    hspec,
    bytestring,
    network,
    word8,
    aeson,
    stm,
    text

  other-modules:
    ParserSpec
    MsgSpec
    PingSpec
    PongSpec
    OkSpec
    ErrSpec
    InfoSpec
    PubSpec
    ConnectSpec
    SubSpec
    UnsubSpec
    Fixtures
    StreamingSpec
    NKeySpec
```

**Step 3: Run the tests**

Run: `cabal test natskell:test:unit-test --test-options="-m NKeySpec"`
Expected: PASS

**Step 4: Commit**

```bash
git add test/Unit/NKeySpec.hs natskell.cabal
git commit -m "$(cat <<'EOF'
test(auth): add unit tests for NKey signing

Tests base32 decoding, error handling, and signature generation.
EOF
)"
```

---

## Task 6: Store nonce from INFO in Client

**Files:**
- Modify: `client/Client.hs:76-87` (Client data type)
- Modify: `client/Client.hs:104-114` (newClient initialization)
- Modify: `client/Client.hs:290-302` (router function)

**Step 1: Add serverNonce field to Client**

In `client/Client.hs`, update the Client data type:

```haskell
-- | Client is used to interact with the NATS server.
data Client = Client'
                { queue               :: Q QueueItem
                , routes              :: TVar (Map Subject (M.Msg -> IO ()))
                , pings               :: TVar [IO ()]
                , randomGen           :: TVar StdGen
                , serverInfo          :: TMVar I.Info
                , serverNonce         :: TVar (Maybe BS.ByteString)
                , connectionAttempts' :: TVar Int
                , poisonpill          :: TVar Bool
                , exited              :: TMVar ()
                , config              :: Config
                , conn                :: Conn
                }
```

**Step 2: Initialize serverNonce in newClient**

In `client/Client.hs`, update the client initialization:

```haskell
  client <- liftIO $ Client'
    <$> newQ
    <*> newTVarIO mempty
    <*> newTVarIO []
    <*> (newTVarIO =<< newStdGen)
    <*> newEmptyTMVarIO
    <*> newTVarIO Nothing       -- serverNonce
    <*> newTVarIO (connectionAttempts defaultConfig)
    <*> newTVarIO False
    <*> newEmptyTMVarIO
    <*> pure defaultConfig
    <*> pure c
```

**Step 3: Update router to store nonce**

In `client/Client.hs`, update the router function:

```haskell
router :: Client -> ParsedMessage -> IO ()
router client msg = do
  case msg of
    ParsedMsg a  -> msgRouter client a
    ParsedInfo i -> do
      atomically $ writeTVar (serverNonce client) (I.nonce i)
      connect client
      atomically (putTMVar (serverInfo client) i)
    ParsedPing _ -> pong client
    ParsedPong _ -> sequenceActions (pings client)
    ParsedOk   _ -> return ()
    ParsedErr err -> case E.isFatal err of
      True  -> do
        runClient client . logError $ ("Fatal error: " ++ show err)
        Client.close client
      False -> runClient client . logWarn $ ("Error: " ++ show err)
```

**Step 4: Build to verify compilation**

Run: `cabal build`
Expected: Build succeeds

**Step 5: Commit**

```bash
git add client/Client.hs
git commit -m "$(cat <<'EOF'
feat(auth): store server nonce from INFO message

The nonce from the INFO message is now stored in the Client for use
in NKey signature generation during CONNECT.
EOF
)"
```

---

## Task 7: Sign nonce in connect function

**Files:**
- Modify: `client/Client.hs:357-361` (connect function)
- Add import: `Crypto.NKey`

**Step 1: Add import for Crypto.NKey**

In `client/Client.hs`, add to imports:

```haskell
import           Crypto.NKey            (signNonce)
```

**Step 2: Update connect function to handle JWTWithNKey**

In `client/Client.hs`, replace the connect function:

```haskell
connect :: Client -> IO ()
connect client = do
  runClient client $ logDebug "Connecting to the NATS server"
  let cfg = config client
  let baseConnect = connectConfig cfg

  dat <- case auth cfg of
    JWTWithNKey jwt nkey -> do
      maybeNonce <- readTVarIO (serverNonce client)
      case maybeNonce of
        Nothing -> do
          runClient client $ logError "No nonce available for NKey signing"
          return baseConnect { Connect.jwt = Just jwt }
        Just nonceVal -> do
          case signNonce nkey nonceVal of
            Left e -> do
              runClient client $ logError ("NKey signing failed: " ++ show e)
              return baseConnect { Connect.jwt = Just jwt }
            Right signature -> do
              runClient client $ logDebug "Signed nonce with NKey"
              return baseConnect { Connect.jwt = Just jwt, Connect.sig = Just signature }

    JWT jwt -> return baseConnect { Connect.jwt = Just jwt }

    NKey nkey -> do
      maybeNonce <- readTVarIO (serverNonce client)
      case maybeNonce of
        Nothing -> do
          runClient client $ logError "No nonce available for NKey signing"
          return baseConnect
        Just nonceVal -> do
          case signNonce nkey nonceVal of
            Left e -> do
              runClient client $ logError ("NKey signing failed: " ++ show e)
              return baseConnect
            Right signature -> do
              runClient client $ logDebug "Signed nonce with NKey"
              return baseConnect { Connect.sig = Just signature }

    UserPass (u, p) -> return baseConnect { Connect.user = Just u, Connect.pass = Just p }

    AuthToken token -> return baseConnect { Connect.auth_token = Just token }

    TLSCert _ -> return baseConnect { Connect.tls_required = True }

    None -> return baseConnect

  writeToClientQueue client (QueueItem dat)
```

**Step 3: Build to verify compilation**

Run: `cabal build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add client/Client.hs
git commit -m "$(cat <<'EOF'
feat(auth): sign server nonce with NKey during CONNECT

When using JWTWithNKey or NKey auth, the connect function now signs
the server nonce with the NKey seed and includes the signature in
the CONNECT message.
EOF
)"
```

---

## Task 8: Run full test suite

**Step 1: Run unit tests**

Run: `cabal test natskell:test:unit-test`
Expected: All tests pass

**Step 2: Run fuzz tests**

Run: `cabal test natskell:test:fuzz-test`
Expected: All tests pass

**Step 3: Run nix flake check**

Run: `nix flake check`
Expected: All checks pass (including hlint and stylish-haskell)

**Step 4: Fix any style issues**

Run: `stylish-haskell -r -i -c stylish.yaml .`

**Step 5: Commit any style fixes**

```bash
git add -A
git commit -m "style: apply stylish-haskell formatting"
```

---

## Task 9: Update module exports

**Files:**
- Modify: `client/Client.hs:7-37` (module exports)

**Step 1: Ensure all new types are exported**

In `client/Client.hs`, verify the export list includes:

```haskell
module Client (
  Client,
  MsgView (..),
  newClient,
  publish,
  subscribe,
  unsubscribe,
  ping,
  Client.close,
  withPayload,
  withReplyCallback,
  withHeaders,
  withConnectName,
  withAuthToken,
  withUserPass,
  withNKey,
  withJWT,
  withJWTCreds,    -- NEW
  withTLSCert,
  withLoggerConfig,
  withConnectionAttempts,
  withExitAction,
  LoggerConfig (..),
  LogLevel (..),
  AuthTokenData,
  UserPassData,
  NKeyData,
  JWTTokenData,
  TLSPublicKey,
  TLSPrivateKey,
  TLSCertData,
  ) where
```

**Step 2: Build and verify**

Run: `cabal build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add client/Client.hs
git commit -m "$(cat <<'EOF'
feat(auth): export withJWTCreds from Client module

Users can now use withJWTCreds to configure combined JWT + NKey auth.
EOF
)"
```

---

## Task 10: End-to-end test with NATS JWT auth

**Files:**
- Create: `test/System/AuthSpec.hs`
- Create: `test/System/fixtures/` (auth credentials)
- Modify: `test/System/Main.hs` (import new spec)
- Modify: `natskell.cabal` (add AuthSpec to other-modules)

**Step 1: Generate test credentials**

Use `nsc` to generate operator, account, and user credentials for testing. Store these in `test/System/fixtures/`:

```bash
mkdir -p test/System/fixtures
cd test/System/fixtures

# Create operator
nsc add operator TestOperator

# Create account
nsc add account TestAccount

# Create user with NKey
nsc add user TestUser

# Export the credentials we need:
# - resolver.conf (for NATS server config)
# - TestUser.jwt (user JWT token)
# - TestUser.nk (user NKey seed)
nsc generate config --nats-resolver > resolver.conf
nsc describe user TestUser -R > TestUser.jwt
# NKey seed needs to be extracted from the creds file
```

Alternatively, create static test credentials that are checked into the repo (these are test-only, not production secrets).

**Step 2: Create AuthSpec.hs**

Create `test/System/AuthSpec.hs` following the existing ClientSpec pattern:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module AuthSpec (spec) where

import           Client
import           Control.Concurrent.STM
import qualified Data.ByteString        as BS
import           Data.UUID              (toString)
import           Data.UUID.V4           (nextRandom)
import           Test.Hspec
import qualified TestContainers.Hspec   as TC

data AuthEndpoints = AuthEndpoints
                       { authNatsHost :: String
                       , authNatsPort :: Int
                       }

-- NATS container configured with JWT auth
authContainer :: TC.TestContainer AuthEndpoints
authContainer = do
    -- Read resolver config from fixtures
    natsContainer <- TC.run (TC.containerRequest
      (TC.fromTag "nats:latest")
      TC.& TC.setExpose [ 4222 ]
      TC.& TC.setWaitingFor (TC.waitUntilMappedPortReachable 4222)
      -- Mount the resolver config and enable auth
      TC.& TC.setVolumeMounts [("test/System/fixtures", "/config")]
      TC.& TC.setCmd ["-c", "/config/resolver.conf", "-DV"]
      )
    pure $ AuthEndpoints
      { authNatsHost = "0.0.0.0"
      , authNatsPort = TC.containerPort natsContainer 4222
      }

-- Test JWT and NKey from fixtures
testJWT :: BS.ByteString
testJWT = "..." -- Load from fixtures/TestUser.jwt

testNKey :: BS.ByteString
testNKey = "..." -- Load from fixtures/TestUser.nk

spec :: Spec
spec = do
  describe "JWT + NKey authentication" $ do
    around (TC.withContainers authContainer) $ do
      it "connects successfully with valid JWT and NKey" $ \(AuthEndpoints host port) -> do
        exitCalled <- newTVarIO False
        client <- newClient [(host, port)]
          [ withJWTCreds testJWT testNKey
          , withExitAction (atomically $ writeTVar exitCalled True)
          ]
        -- Verify connection succeeded by doing a ping
        pongReceived <- newEmptyTMVarIO
        ping client (atomically $ putTMVar pongReceived ())
        atomically $ takeTMVar pongReceived
        Client.close client

      it "fails to connect with invalid NKey" $ \(AuthEndpoints host port) -> do
        exitCalled <- newTVarIO False
        client <- newClient [(host, port)]
          [ withJWTCreds testJWT "INVALID_NKEY_SEED"
          , withExitAction (atomically $ writeTVar exitCalled True)
          ]
        -- Should fail to connect - exit action should be called
        atomically $ check =<< readTVar exitCalled
```

**Step 3: Update Main.hs**

In `test/System/Main.hs`, import the new spec:

```haskell
import qualified AuthSpec
import qualified ClientSpec

main :: IO ()
main = hspec $ do
  ClientSpec.spec
  AuthSpec.spec
```

**Step 4: Update cabal file**

In `natskell.cabal`, add `AuthSpec` to system-test other-modules:

```cabal
test-suite system-test
  other-modules:
      ClientSpec
      AuthSpec
```

**Step 5: Run the end-to-end test**

Run: `cabal test natskell:test:system-test -fimpure`
Expected: All tests pass, including the new auth tests

**Step 6: Commit**

```bash
git add test/System/AuthSpec.hs test/System/fixtures/ test/System/Main.hs natskell.cabal
git commit -m "$(cat <<'EOF'
test(auth): add end-to-end tests for JWT + NKey authentication

Tests the full authentication flow against a real NATS server configured
with JWT auth. Verifies both successful auth and rejection of invalid
credentials.
EOF
)"
```

---

## Summary

After completing all tasks, the library will support:

1. `withJWT jwt` - JWT-only auth (existing)
2. `withNKey nkey` - NKey-only auth with nonce signing (enhanced)
3. `withJWTCreds jwt nkey` - Combined JWT + NKey auth with nonce signing (new)

Example usage:

```haskell
import Client

main = do
  let jwt = "eyJ0eXAiOiJKV1Q..."
  let nkey = "SUACSSL3UAHUDXKFSNVUZRF5UHPMWZ6BFDTJ7M6USDXIIEQHP5D6HCQHBU"

  client <- newClient [("localhost", 4222)] [withJWTCreds jwt nkey]
  -- Client will:
  -- 1. Receive INFO with nonce from server
  -- 2. Sign nonce with NKey seed
  -- 3. Send CONNECT with jwt and sig fields
```
