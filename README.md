# Natskell

Natskell is a client library for [NATS](https://docs.nats.io/) written in haskell

# Project

See roadmap [here](https://github.com/users/samisagit/projects/1)

## Installation

Natskell is still pre alpha, so there is no candidate at present.

## Usage

The API is not set in stone (yet). In the mean time the tests give a decent idea of what is currently possible.

## Contributing
Pull requests are welcome. Please open an issue first to discuss what you would like to change.

Please make sure to add tests.

Please make sure all commits are signed.

Documentation is built on merge to main and can be found at https://samisagit.github.io/natskell/

### Nix

This project has a nix flake, which sets up a useful dev shell. To use it run `nix develop`, which will install the project system depenencies. 

There is a public cachix store at samisagit-natskell for this flake.

You can run the pure tests with `nix flake check`.

System tests need access to a NATS server, so you will need to run `nix develop --command bash -c "cabal test natskell:test:system-test"` to run them.
