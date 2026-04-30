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
- `cabal test natskell:test:integration-tests -f impure` runs integration tests (skipped without `-f impure`).
- `cabal test system-tests` runs system tests (requires Docker/TestContainers and a reachable NATS image).
- `nix flake check` runs Nix checks in a CI-like mode (tests, lint, formatting, packaging checks).

## Coding Style & Naming Conventions
- Capabilities should be *modular*, consisting of an API and an implementation.
- API modules should consist of a capability record, along with any types needed to interact with said record.
- The implementation modules are only meant to be imported by the client implementation (client/Client.hs), they should implement the capability record.
- All other modules should only know about the relevant API module.
- Haskell2010; module names are `CamelCase` and match file names (e.g., `internal/Parsers/Parsers.hs`).
- Format with `stylish-haskell -vri -c stylish.yaml .` and address `hlint` suggestions.

## Testing Guidelines
- Tests use `hspec` with `hspec-discover` (`Spec.hs` files).
- Naming convention: `*Spec.hs` under `test/Unit`, `test/Fuzz`, `test/Integration`, and `system-tests/test/System`.
- Add tests for behavior changes; prefer unit tests, then integration/system tests for NATS behavior.

## Commit & Pull Request Guidelines
- Never use git unless explicitly instructed.
- flake interactions only work on tracked files, if you need to add a file, ask for it to be added to the flake.

## Interaction with the code base
- A focus on reducing code bloat should be forefront.
- Run a check after making changes to ensure there is no unused code or imports across the codebase.
- Avoid adding new dependencies unless absolutely necessary, and ensure they are well justified and do not bloat the project.

## Workflow Rules
- Read `AGENT_LESSONS.md` before starting work to learn from past issues and solutions.
- Run `hlint .` before prompting for more input.
- Run `stylish-haskell -ri -c stylish.yaml .` before prompting for more input.
- Run `nix flake check` after changes are made, rather than suggesting the user runs tests.
- Run impure (integration and system) tests immediately preceding prompting for more input from the user,
  if something fails, fix the issue before prompting for more input.
- For new features, add unit tests first, then integration tests if necessary.
- If there are breaking changes or behaviour changes, add system tests to verify the expected behaviour of the NATS server with the new client changes.
- When you run into a tooling issue, update the AGENT_LESSONS.md file with the solution to the issue, so that future agents can learn from it and avoid the same issue. If you fail to diagnose the issue, prompt for a user fix.
