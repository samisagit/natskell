{
  description = "A flake to develop Natskell";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-24.05";
    };
    flake-utils.url = "github:numtide/flake-utils/master";
  };


  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        hls = pkgs.haskell-language-server.override{supportedGhcVersions = [ "925" ];};
      in {
        devShells.default = pkgs.mkShell{ 
          buildInputs = [
            hls
            pkgs.hlint
            pkgs.stylish-haskell
	    pkgs.zlib
	    pkgs.cabal-install
	    pkgs.haskell.compiler.ghc925
            pkgs.haskellPackages.hspec-discover
	    pkgs.nodejs_22 # because copilot
          ];
          NIX_PATH = "nixpkgs=" + pkgs.path;
        };
        devShells.ci = pkgs.mkShell{ 
          buildInputs = [
            pkgs.hlint
            pkgs.stylish-haskell
	    pkgs.zlib
	    pkgs.cabal-install
	    pkgs.haskell.compiler.ghc925
            pkgs.haskellPackages.hspec-discover
          ];
          NIX_PATH = "nixpkgs=" + pkgs.path;
        };
      });
}
