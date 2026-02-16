# Repository Guidelines

## Project Structure & Module Organization
- `client/` contains the public `natskell` library API (e.g., `Client` and related types).
- `internal/` holds the `natskell-internal` library (parsers, pipelines, network, and internal types).
- `test/Unit`, `test/Fuzz`, and `test/Integration` contain unit, QuickCheck fuzz, and integration specs.
- `system-tests/` is a separate Cabal package for TestContainers-based system tests.
- `cabal.project*`, `natskell.cabal`, `flake.nix`, and `stylish.yaml` define build tooling and formatting.
- `dist-newstyle/` and `result/` are generated build outputs and should not be edited.

## Build, Test, and Development Commands
- `nix develop` enters the Nix dev shell (cabal, hlint, stylish-haskell, hspec-discover).
- `cabal build all` builds libraries and test suites.
- `cabal test natskell:test:unit-test` runs unit tests.
- `cabal test natskell:test:fuzz-test` runs fuzz tests.
- `cabal test natskell:test:client-test -f impure` runs integration tests (skipped without `-f impure`).
- `cabal --project-file=cabal.project.system-tests test natskell-system-tests` runs system tests (requires Docker/TestContainers and a reachable NATS image).
- `nix flake check` runs Nix checks in a CI-like mode (tests, lint, formatting, packaging checks).

## Coding Style & Naming Conventions
- Haskell2010; module names are `CamelCase` and match file names (e.g., `internal/Parsers/Parsers.hs`).
- Format with `stylish-haskell -ri -c stylish.yaml .` and address `hlint` suggestions.

## Testing Guidelines
- Tests use `hspec` with `hspec-discover` (`Spec.hs` files).
- Naming convention: `*Spec.hs` under `test/Unit`, `test/Fuzz`, `test/Integration`, and `system-tests/test/System`.
- Add tests for behavior changes; prefer unit tests, then integration/system tests for NATS behavior.

## Commit & Pull Request Guidelines
- Never use git unless explicitly instructed.

## Workflow Guidelines
- Run nix flake check after changes are made, rather than suggesting the user runs tests.
- For new features, add unit tests first, then integration tests if necessary.
- If there are breaking changes or behaviour changes, add system tests to verify the expected behaviour of the NATS server with the new client changes.
- Prefer TDD in general.

## Interaction with the code base
- A focus on reducing code bloat should be forefront.
- Run a check after making changes to ensure there is no unused code or imports across the codebase.
- Avoid adding new dependencies unless absolutely necessary, and ensure they are well justified and do not bloat the project.


