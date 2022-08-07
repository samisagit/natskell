{ nixpkgs ? import (fetchTarball "http://nixos.org/channels/nixos-22.11/nixexprs.tar.xz") {}, compiler ? "ghc92", hls ? true }:
let
  inherit (nixpkgs) pkgs;
  ghc = if hls then pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [haskell-language-server]) else pkgs.haskell.packages.${compiler}.ghc;
in
pkgs.mkShell {
  buildInputs = [ ghc pkgs.zlib pkgs.stack pkgs.hlint ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
  NIX_PATH = "nixpkgs=" + pkgs.path;
}
