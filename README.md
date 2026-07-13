# Natskell

Natskell is a client library for [NATS](https://docs.nats.io/) written in haskell

# Project

See roadmap [here](https://github.com/users/samisagit/projects/1)

## Installation

Natskell is available on Hackage at https://hackage.haskell.org/package/natskell

## Usage

Subjects and payloads are `ByteString`. Enable `OverloadedStrings` or use `Data.ByteString.Char8.pack`.

### Basic publish/subscribe

```haskell
{-# LANGUAGE OverloadedStrings #-}

import API (Client (..))
import Client

main :: IO ()
main = do
  result <- newClient [("127.0.0.1", 4222)] [withConnectName "demo"]
  case result of
    Left err -> print err
    Right client -> do
      sid <- subscribe client "updates" [] print
      publish client "updates" [withPayload "hello"]
      unsubscribe client sid
      close client
```

### Request/reply

```haskell
publish client "service.echo" [withPayload "hello", withReplyCallback print]
```

### Authentication and TLS

Static authentication is available through `withAuthToken`, `withUserPass`,
`withNKey`, and `withJWT`. The corresponding handler options refresh secrets on
every connection and reconnection: `withAuthTokenHandler`,
`withUserPassHandler`, `withNKeyHandler`, and `withJWTHandlers`. NKey and JWT
signature handlers return the raw Ed25519 signature; Natskell performs the NATS
base64url encoding.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Client
import qualified Data.ByteString as BS

main :: IO ()
main = do
  caPem <- BS.readFile "ca.pem"
  let opts =
        [ withUserPass ("alice", "secret")
        , withTLSRootCA caPem
        ]
  result <- newClient [("nats.internal", 4222)] opts
  case result of
    Left err -> print err
    Right client -> close client
```

TLS verifies the server certificate against the operating-system trust store by
default. `withTLSRootCA` adds a private CA, `withTLSServerName` overrides the
verification/SNI name, and `withTLSCert` supplies a client certificate and key
for mutual TLS. `withTLS` enables TLS without additional material.
`withTLSInsecure` disables verification and should only be used in controlled
test environments.

### JetStream

JetStream is part of the main `natskell` library. No separate Cabal component is
required; import the nested `JetStream.*` modules directly.

```haskell
import JetStream.Client (newJetStream)

-- Given a successfully connected NATS client:
let jetStream = newJetStream client []
```

The capability records and types are exposed from `JetStream.API`, with stream,
consumer, publish, and message operations under their corresponding
`JetStream.*` modules.

### Exit handling

```haskell
let opts =
      [ withExitAction (\reason -> putStrLn ("client exit: " ++ show reason))
      ]
result <- newClient [("127.0.0.1", 4222)] opts
```

`newClient` waits for the initial INFO/TLS/CONNECT/PING/PONG handshake and
returns `Left ConnectError` if every configured endpoint fails. Later terminal
disconnects are reported to `withExitAction`.

Configuration options are applied in declaration order; when options overlap,
the last option wins.

## Contributing
Pull requests are welcome. Please open an issue first to discuss what you would like to change.

Please make sure to add tests.

Please make sure all commits are signed.

Documentation is built on merge to main and can be found at https://samisagit.github.io/natskell/

### Nix

This project has a nix flake, which sets up a useful dev shell. To use it run `nix develop`, which will install the project system depenencies. 

There is a public cachix store at samisagit-natskell for this flake.

You can run the pure tests with `nix flake check`.

Integration tests need access to a socket, so you will need to run `nix develop "cabal test -fimpure integration-tests"` to run them.

System tests need access to a NATS server, so you will need to run `nix develop "cabal test ./system-tests"` to run them.
