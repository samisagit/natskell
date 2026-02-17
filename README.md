# Natskell

Natskell is a client library for [NATS](https://docs.nats.io/) written in haskell

# Project

See roadmap [here](https://github.com/users/samisagit/projects/1)

## Installation

Natskell is still pre alpha, so there is no candidate at present.

## Usage

Subjects and payloads are `ByteString`. Enable `OverloadedStrings` or use `Data.ByteString.Char8.pack`.

### Basic publish/subscribe

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Client

main :: IO ()
main = do
  client <- newClient [("127.0.0.1", 4222)] [withConnectName "demo"]
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

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Client
import qualified Data.ByteString as BS

main :: IO ()
main = do
  certPem <- BS.readFile "client.pem"
  keyPem <- BS.readFile "client.key"
  let opts =
        [ withUserPass ("alice", "secret")
        , withTLSCert (certPem, keyPem)
        ]
  _ <- newClient [("127.0.0.1", 4222)] opts
  pure ()
```

Other auth helpers: `withAuthToken`, `withNKey`, `withJWT`.

### Exit handling

```haskell
let opts =
      [ withExitAction (\reason -> putStrLn ("client exit: " ++ show reason))
      ]
client <- newClient [("127.0.0.1", 4222)] opts
```

## Contributing
Pull requests are welcome. Please open an issue first to discuss what you would like to change.

Please make sure to add tests.

Please make sure all commits are signed.

Documentation is built on merge to main and can be found at https://samisagit.github.io/natskell/

### Nix

This project has a nix flake, which sets up a useful dev shell. To use it run `nix develop`, which will install the project system depenencies. 

There is a public cachix store at samisagit-natskell for this flake.

You can run the pure tests with `nix flake check`.

System tests need access to a NATS server, so you will need to run `nix develop --command bash -c "cabal --project-file=cabal.project.system-tests test natskell-system-tests"` to run them.
