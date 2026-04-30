# ADR 0001: Abstract Parser Behavior Behind a Backend-Agnostic Protocol Port

- Status: Proposed
- Date: 2026-04-29

## Context

The current parser seam is not a real backend abstraction.

- [Parser.API](/home/sam/repos/natskell/internal/Plumbing/Parser/API.hs) exposes the custom parser type, custom error type, and recovery suggestions.
- [Pipeline.Streaming.Parser](/home/sam/repos/natskell/internal/Plumbing/Pipeline/Streaming/Parser.hs) knows how to interpret `ParserErr` and `solveErr` directly.
- [Handshake.Nats](/home/sam/repos/natskell/internal/Policy/Handshake/Nats.hs) also knows how to interpret `solveErr` while reading the initial `INFO`.

That means the streaming and handshake code are coupled to the current parser engine instead of depending on a smaller description of parser behavior.

The current requirements are:

- strict `ByteString` input
- incremental consumption of network data
- correct handling of messages split across frames
- preservation of unconsumed suffix bytes after a successful parse
- current recovery behavior for garbled prefix bytes and partial protocol prefixes
- no parser-level knowledge of connection lifecycle or buffer limit policy
- compatibility with the project GHC matrix, currently [8.8 .. 9.12] in [natskell.cabal](/home/sam/repos/natskell/natskell.cabal)

We want to keep the current parser implementation available, but also allow a prebuilt backend that can satisfy the same behavior with less custom parsing code.

## Decision

Introduce a backend-agnostic protocol parser port and make handshake and streaming depend only on that port.

The port should describe parser behavior, not parser internals. A representative shape is:

```haskell
data ParseStep a
  = Emit a ByteString
  | NeedMore
  | DropPrefix Int String
  | Reject String

newtype ProtocolParser a = ProtocolParser
  { step :: ByteString -> ParseStep a
  }
```

Notes:

- `Emit` returns one parsed message plus the unconsumed suffix.
- `NeedMore` means the current buffer may become valid with more input.
- `DropPrefix` means the buffer contains an invalid prefix and the caller should discard the given number of bytes before trying again.
- `Reject` means the parser cannot recover locally and the caller should treat it as a protocol failure.

This port deliberately does not expose:

- the current `Parser` type
- the current `ParserErr` type
- the current `Suggestion` type
- attoparsec `IResult`
- any parser-library-specific continuation type

### Backends

Two backends should satisfy this port.

#### 1. Current custom parser backend

The existing parser remains supported by adapting:

- `genericParse`
- `solveErr`

into `ProtocolParser`.

This preserves current behavior without requiring a grammar rewrite.

#### 2. Attoparsec backend

The recommended prebuilt backend is `attoparsec`, specifically strict `ByteString` parsing via `Data.Attoparsec.ByteString`.

The attoparsec adapter should map:

- `Done rest value` to `Emit value rest`
- `Partial _` to `NeedMore`
- `Fail _ _ msg` to `DropPrefix` or `Reject`, via shared NATS resynchronization rules

## Why Attoparsec

`attoparsec` is the selected prebuilt backend because it is the best fit for the current requirements with minimal architectural change.

Reasons:

- It is explicitly designed for efficient `ByteString` parsing.
- It supports incremental input natively.
- It is aimed at network and protocol parsing workloads.
- Its result model already distinguishes successful parse, failure, and suspended parse waiting for more input.

This selection is an engineering inference from the library documentation and the current client requirements.

## Why Not Flatparse Right Now

`flatparse` remains an interesting future option, but it is not the selected backend for this ADR.

Reasons:

- Its own documentation says it does not support incremental parsing.
- Current handshake and streaming behavior rely on partial-input parsing over incoming network chunks.
- Choosing it now would likely force caller redesign or a more invasive buffering adapter than attoparsec requires.
- Its published Hackage metadata currently lists testing through GHC 9.8.4, while this project tests through 9.12.

`flatparse` may still be worth benchmarking later if we first stabilize the backend-agnostic protocol parser port.

## Resynchronization Policy

Resynchronization is protocol policy, not parser-library policy.

The current system recovers from several malformed-input cases by dropping invalid prefix bytes and trying again. That behavior is already asserted in integration tests.

Therefore:

- the parser backend must report whether more input is needed or bytes should be dropped
- the NATS-specific resynchronization logic should be shared behavior above the parser engine
- handshake and streaming should not know the details of custom parser errors or attoparsec failures

This keeps the backend swap boundary small while preserving current recovery semantics.

## Consequences

### Positive

- handshake and streaming no longer depend on the custom parser error model
- the current parser can remain in place behind an adapter
- attoparsec can be introduced without changing the client surface
- backend-neutral tests can target protocol behavior instead of parser internals
- future parser benchmarking becomes simpler

### Negative

- one more adapter layer is introduced
- attoparsec failure handling will still need explicit NATS resynchronization rules
- some current parser-combinator tests will remain implementation-specific unless separately redesigned

## Alternatives Considered

### Keep the current `Parser.API` shape

Rejected.

It leaks implementation details into consumers and does not create a true backend seam.

### Replace the parser directly with attoparsec and skip the abstraction

Rejected.

That would couple the rest of the codebase to attoparsec result handling and repeat the same mistake in a new library.

### Choose flatparse as the primary backend

Rejected for now.

It may be faster on paper, but it does not natively satisfy the current incremental-input requirement.

### Choose megaparsec or parsec

Rejected.

They are weaker fits for the current network-oriented, `ByteString`-based, partial-input parsing workload than attoparsec.

## Implementation Notes

When this ADR is implemented:

1. Replace the parser port consumed by streaming and handshake with the backend-agnostic behavior port.
2. Move recovery interpretation out of [Pipeline.Streaming.Parser](/home/sam/repos/natskell/internal/Plumbing/Pipeline/Streaming/Parser.hs) and [Handshake.Nats](/home/sam/repos/natskell/internal/Policy/Handshake/Nats.hs).
3. Keep the current custom parser as the first adapter.
4. Add an attoparsec-backed adapter for the same `ParsedMessage` domain.
5. Preserve existing integration behavior for:
   - garbled prefixes
   - partial protocol prefixes
   - split frames
   - oversized inbound messages

## References

- attoparsec `Data.Attoparsec.ByteString` documentation:
  https://hackage.haskell.org/package/attoparsec/docs/Data-Attoparsec-ByteString.html
- attoparsec package page:
  https://hackage.haskell.org/package/attoparsec
- flatparse package page and README:
  https://hackage.haskell.org/package/flatparse
