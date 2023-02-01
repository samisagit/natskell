{
  description = "A flake to develop Natskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
  };

  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc92";
        ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [haskell-language-server]);
        pkgs = import nixpkgs {
          inherit system;
        };
      in {
	defaultPackage= pkgs.hello;

        devShell = pkgs.mkShell{ 
          buildInputs = [
            ghc
            pkgs.stack
            pkgs.hlint
            pkgs.stylish-haskell
	    pkgs.zlib
	    pkgs.gnumake
          ];
          shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
          NIX_PATH = "nixpkgs=" + pkgs.path;
        };
      });
}
