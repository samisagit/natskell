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

import Nats

main :: IO ()
main = do
  endpoint <- either (fail . show) pure (server "127.0.0.1" 4222)
  result <- connect [endpoint] [withConnectName "demo"]
  case result of
    Left err -> print err
    Right client -> do
      subscription <- subscribe client "updates" [] print
      _ <- publish client "updates" "hello" []
      case subscription of
        Left err     -> print err
        Right handle -> do
          _ <- unsubscribe client handle []
          pure ()
      close client []
```

### Request/reply

```haskell
reply <- request client "service.echo" "hello" [withRequestTimeout 2]
```

`request` creates an inbox subscription, publishes the payload, waits for one
reply, and cleans up the subscription on replies, timeouts, connection closure,
and exceptions.

### Authentication and TLS

Static authentication is available through `withAuthToken`, `withUserPass`,
`withNKey`, and `withJWT`. The corresponding handler options refresh secrets on
every connection and reconnection: `withAuthTokenHandler`,
`withUserPassHandler`, `withNKeyHandler`, and `withJWTHandlers`. NKey and JWT
signature handlers return the raw Ed25519 signature; Natskell performs the NATS
base64url encoding.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Nats
import qualified Data.ByteString as BS

main :: IO ()
main = do
  caPem <- BS.readFile "ca.pem"
  endpoint <- either (fail . show) pure (server "nats.internal" 4222)
  let opts =
        [ withUserPass ("alice", "secret")
        , withTLSRootCA caPem
        ]
  result <- connect [endpoint] opts
  case result of
    Left err -> print err
    Right client -> close client []
```

TLS verifies the server certificate against the operating-system trust store by
default. Configuring one or more private roots with `withTLSRootCA` replaces the
operating-system roots for that client connection; it never modifies the host's
trust store. `withTLSServerName` overrides the verification/SNI name, and
`withTLSCert` supplies a client certificate and key for mutual TLS. `withTLS`
enables TLS without additional material.
`withTLSInsecure` disables verification and should only be used in controlled
test environments.

### JetStream

JetStream is part of the main `natskell` library. No separate package is
required; import it through the nested `Nats.JetStream.*` modules.

```haskell
import qualified Nats.JetStream as JetStream

-- Given a successfully connected NATS client:
case JetStream.newJetStream client [] of
  Left configError -> print configError
  Right jetStream  -> do
    account <- JetStream.accountInfo jetStream []
    print account
```

Stream, consumer, publish, and message operations live under
`Nats.JetStream.Stream`, `Nats.JetStream.Consumer`,
`Nats.JetStream.Publish`, and `Nats.JetStream.Message`. Every network operation
has a final request-options argument, so future call options can be added
without changing its type.

### Exit handling

```haskell
let opts =
      [ withExitAction (\reason -> putStrLn ("client exit: " ++ show reason))
      ]
endpoint <- either (fail . show) pure (server "127.0.0.1" 4222)
result <- connect [endpoint] opts
```

`connect` waits for the initial INFO/TLS/CONNECT/PING/PONG handshake and
returns `Left ConnectError` if every configured endpoint fails. Later terminal
disconnects are reported to `withExitAction`.

Configuration options are applied in declaration order; when options overlap,
the last option wins.

### Public API stability

New code should use the canonical `Nats.*` modules. Public clients,
subscriptions, capability groups, resource handles, messages, and response
records are abstract and are inspected through accessors. Core message payloads
are strict `ByteString` values; an empty NATS payload is represented by an empty
`ByteString`, not `Nothing`.

Operations return explicit `Either` errors and reserve final option-list
arguments for additive evolution. Sequence numbers use the abstract
`Sequence` type backed by `Word64`, and server-defined JetStream enums preserve
unknown wire values. The historical `API`, `Client`, and `JetStream.*` imports
remain available as compatibility module paths, while `Nats.*` is the preferred
public namespace.

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
