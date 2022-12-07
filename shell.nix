{ nixpkgs ? import (fetchTarball "http://nixos.org/channels/nixos-22.11/nixexprs.tar.xz") {}, compiler ? "ghc92" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          haskell-language-server
	  stack
        ]);
in
pkgs.mkShell {
  buildInputs = [ ghc pkgs.zlib pkgs.stack pkgs.hlint ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
