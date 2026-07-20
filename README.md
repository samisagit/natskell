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

import qualified API as Nats
import qualified Client

main :: IO ()
main = do
  endpoint <- either (fail . show) pure (Client.server "127.0.0.1" 4222)
  result <- Client.connect [endpoint] [Client.withConnectName "demo"]
  case result of
    Left err -> print err
    Right client -> do
      subscription <- Nats.subscribe client "updates" [] print
      _ <- Nats.publish client "updates" "hello" []
      case subscription of
        Left err     -> print err
        Right handle -> do
          _ <- Nats.unsubscribe client handle []
          pure ()
      Nats.close client []
```

### Request/reply

```haskell
reply <- Nats.request client "service.echo" "hello" [Nats.withRequestTimeout 2]
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

import qualified API as Nats
import qualified Client
import qualified Data.ByteString as BS

main :: IO ()
main = do
  caPem <- BS.readFile "ca.pem"
  endpoint <- either (fail . show) pure (Client.server "nats.internal" 4222)
  let opts =
        [ Client.withUserPass ("alice", "secret")
        , Client.withTLSRootCA caPem
        ]
  result <- Client.connect [endpoint] opts
  case result of
    Left err -> print err
    Right client -> Nats.close client []
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
required. `JetStream.API` reexports the complete JetStream contract; its child
modules are available when narrower imports are preferable.

```haskell
import qualified JetStream.API as JetStream
import qualified JetStream.Client as JetStreamClient

-- Given a successfully connected NATS client:
case JetStreamClient.newJetStream client [] of
  Left configError -> print configError
  Right jetStream  -> do
    account <- JetStream.accountInfo (JetStream.management jetStream) []
    print account
```

The focused contracts live under `JetStream.API.Stream`,
`JetStream.API.Consumer`, `JetStream.API.Publish`, `JetStream.API.Message`,
`JetStream.API.Management`, and `JetStream.API.KeyValue`. Every network
operation has a final request-options argument, so future call options can be
added without changing its type.

Key-value buckets use the standard JetStream `KV_<bucket>` stream and
`$KV.<bucket>.>` subjects. Bucket and key names are validated before network
I/O. Values are strict `ByteString` payloads. `putKeyValueEntry` writes
unconditionally, while `createKeyValueEntry`, `updateKeyValueEntry`, and the
optional `withKeyValueLastRevision` delete guard use JetStream subject-sequence
compare-and-set semantics.

```haskell
bucketResult <- JetStream.createKeyValueBucket
  (JetStream.keyValues jetStream)
  "PROCESS_STATE"
  [ JetStream.withKeyValueHistory 3
  , JetStream.withKeyValueMaxValueSize (1024 * 1024)
  , JetStream.withKeyValueStorage JetStream.FileStorage
  ]
  []

case bucketResult of
  Left err -> print err
  Right bucket -> do
    revision <- JetStream.putKeyValueEntry
      (JetStream.keyValues jetStream)
      bucket
      "tenant/process-instance"
      binaryState
      []
    print revision
```

Watches are pull-based ordered consumers. `fetchKeyValueWatch` returns entries,
the pull status, and `keyValueWatchInitialComplete`, which becomes true once the
stable initial snapshot has arrived. Watch options support latest values,
history, updates only, delete filtering, and metadata-only delivery. Bucket
history is limited to 64 revisions per key, matching NATS clients.
Bucket creation requires NATS Server 2.7.2 or later. S2 compression requires
NATS Server 2.10.0 or later.

Stream configuration supports a per-message encoded-size limit with
`withMaxMessageSize`; the limit includes headers as well as payload bytes, and
`-1` means unlimited.

On NATS Server 2.7.1 or later, `withConsumerBackoff` configures redelivery after
acknowledgment timeouts. It does not affect explicit NAK redelivery. The first
delay overrides `AckWait`, later entries apply to successive redeliveries, and
the final entry repeats after the schedule is exhausted. A non-empty schedule
cannot be longer than a positive `MaxDeliver`. Durations are converted with
exact arithmetic and floored to whole nanoseconds, so a non-negative duration
below one nanosecond becomes zero; the wire values must fit signed 64-bit
nanoseconds.

`withConsumerMaxRequestBatch` caps pull-request batch size on NATS Server 2.7.0
or later and is only meaningful for pull consumers. Empty backoff and a zero
batch limit leave their server settings unset. Stream and consumer info expose
effective values through `streamConfigMaxMessageSize`, `consumerConfigBackoff`,
and `consumerConfigMaxRequestBatch`.

The existing `ack`, `nak`, `inProgress`, and `term` dispositions are
asynchronous when their request-options list is empty. Supplying a request
option to one of these operations or to `nakWithDelay` asks JetStream to confirm
the disposition; `ackSync` always requests confirmation. `termSync` provides
the same guarantee for terminal dispositions as a natskell convenience.
Delayed redelivery requires NATS Server 2.7.1 or newer and uses a validated,
whole-nanosecond duration:

```haskell
case JetStream.nakDelay 30 of
  Nothing -> pure ()
  Just delay ->
    JetStream.nakWithDelay (JetStream.messages jetStream) message delay []
```

`nakDelay` rejects durations below one nanosecond and values outside the
JetStream signed 64-bit nanosecond range.

### Exit handling

```haskell
let opts =
      [ Client.withExitAction (\reason -> putStrLn ("client exit: " ++ show reason))
      ]
endpoint <- either (fail . show) pure (Client.server "127.0.0.1" 4222)
result <- Client.connect [endpoint] opts
```

`connect` waits for the initial INFO/TLS/CONNECT/PING/PONG handshake and
returns `Left ConnectError` if every configured endpoint fails. Later terminal
disconnects are reported to `withExitAction`.

### Connection lifecycle and protocol errors

`Nats.connectionState` reports `ConnectionConnecting`,
`ConnectionConnected`, `ConnectionReconnecting`, `ConnectionClosing reason`,
or `ConnectionClosed reason`. Publish, subscribe, request, ping, and flush
operations issued during a reconnect wait for the next ready connection or
return `NatsConnectionClosed` if the client becomes terminal. Unsubscribe
removes local subscription state immediately; the reconnect boundary removes
its old server-side subscription.

A successful core `publish` means the at-most-once command was queued. Buffered
publishes may survive a reconnect. If the next server negotiates a smaller
`max_payload`, oversized buffered publishes are dropped and reported through
`Client.withErrorHandler` rather than written to that server.

Use `Client.withConnectionEventHandler` for serial notifications after a
connection disconnects, reconnects, or closes. Closed events include the
terminal `ClientExitReason`; the initial connection does not emit a reconnect
event. Polling `Nats.connectionState` remains available for snapshots.

`ping` and `flush` have finite two-second defaults. Override them per call with
`Nats.withPingTimeout` and `Nats.withFlushTimeout`. A timeout or cancellation
resets the current connection before reconnecting, so an old PONG cannot
complete a later call.

Use `Client.withServerErrorHandler` to receive typed server `-ERR` values,
including non-fatal permission errors. `Client.serverErrorKind` classifies
authentication, permission, protocol, resource-limit, connection, and unknown
errors without requiring callers to inspect the server's text. Handlers run on
a dedicated serial worker rather than the socket reader. The server-error
backlog is bounded; lifecycle transitions have reserved delivery and preserve
their order with server errors. Protocol work such as PONG handling continues
immediately. Excess non-fatal server errors are dropped instead of blocking or
resetting the connection. Fatal errors are still reported through
`withExitAction` and close the client.

Configuration options are applied in declaration order; when options overlap,
the last option wins.

### Public API stability

The public package deliberately exposes only `API`, `Client`, `JetStream.API`,
the six focused `JetStream.API.*` contracts, and `JetStream.Client`. Public
clients, subscriptions, capability groups, resource handles, messages, and
response records are abstract and are inspected through accessors. Core message
payloads are strict `ByteString` values; an empty NATS payload is represented by
an empty `ByteString`, not `Nothing`.

Operations return explicit `Either` errors and reserve final option-list
arguments for additive evolution. JetStream sequence numbers use `Word64`, and
server-defined JetStream enums preserve unknown wire values.

The private `natskell-internal` sublibrary shares implementation code with the
test suites. Its modules and constructors are intentionally unstable and are
not part of the public compatibility contract above.

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
