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
        apps.stack-test = let
          stack-test = pkgs.writeShellApplication {
            name = "stack-test";
            runtimeInputs = [
	      # TODO: this will likely fail without pkg-config, zlib or hspec-discover, but lets see
	      pkgs.stack
	    ];
            text = ''
	    stack test --resolver="$RESOLVER"
	    '';
          };
        in {
          type = "app";
          program = "${stack-test}/bin/stack-test";
        };

        apps.cabal-test = let
          cabal-test = pkgs.writeShellApplication {
            name = "cabal-test";
            runtimeInputs = [
              pkgs.haskellPackages.hspec-discover
	      pkgs.zlib
	      pkgs.cabal-install
	      pkgs.haskell.compiler.ghc925
	      pkgs.pkg-config
	      pkgs.cabal-install
            ];
            text = ''
            export PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig"
	    cabal update
	    cabal test --enable-tests
	    '';
          };
        in {
          type = "app";
          program = "${cabal-test}/bin/cabal-test";
        };

        apps.cabal-check = let
          cabal-check = pkgs.writeShellApplication {
            name = "cabal-check";
            runtimeInputs = [
	      pkgs.zlib
	      pkgs.cabal-install
	      pkgs.pkg-config
	      pkgs.cabal-install
            ];
            text = ''
            export PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig"
	    cabal check
	    '';
          };
        in {
          type = "app";
          program = "${cabal-check}/bin/cabal-check";
        };

        apps.lint = let
          lint = pkgs.writeShellApplication {
            name = "lint";
            runtimeInputs = [
              pkgs.hlint
              pkgs.stylish-haskell
            ];
            text = ''
	    stylish-haskell -r -c stylish.yaml .
	    hlint --git
	    '';
          };
        in {
          type = "app";
          program = "${lint}/bin/lint";
        };

        devShells.default = pkgs.mkShell{ 
          buildInputs = [
            hls
            pkgs.hlint
            pkgs.stylish-haskell
	    pkgs.cabal-install
	    pkgs.zlib
	    pkgs.haskell.compiler.ghc925
            pkgs.haskellPackages.hspec-discover
	    pkgs.nodejs_22 # because copilot
	    pkgs.pkg-config
          ];
          NIX_PATH = "nixpkgs=" + pkgs.path;
          PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig";
        };
      });
}
