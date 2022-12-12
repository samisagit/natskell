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

### Nix
The project has a `shell.nix` that by default sets up a useful environment to develop in, including ghc, stack, hls and hlint.

```
nix-shell # downloads the dependencies (or reads the cache) and adds them to your path temporarily
```

See [here](https://nixos.org/manual/nix/stable/command-ref/nix-shell.html) for more details and [here](https://nixos.org/download.html) to install

Fair warning - compiling the LSP is _not_ a short process (an hour and 15 minutes on an i7-1165G7), but you need haskell-language-server compiled by the same GHC that is used in stack. You can disable the LSP compilation with `--arg hls false` and install the relevant precompiled LSP yourself if this causes an issue.

For a more persistent env try putting the below in your nix config

```
{
  packageOverrides = pkgs: with pkgs; {
    myGHC = pkgs.haskell.packages.ghc92.ghcWithPackages (ps: with ps; [
      haskell-language-server
      hlint
      stylish-haskell
    ]);
    myHaskellPackages = pkgs.buildEnv {
      name = "my-haskell-packages";
      paths = [
        zlib
        stack
      ];
    };
  };
}
```

and running

```bash
nix-env -iA nixpkgs.myGHC
nix-env -iA nixpkgs.myHaskellPackages
```
