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
            ];
            text = ''
            export PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig"
	    cabal update
	    cabal test --enable-tests --test-show-details=direct
	    '';
          };
        in {
          type = "app";
          program = "${cabal-test}/bin/cabal-test";
        };

        apps.cabal-unit-test = let
          cabal-unit-test = pkgs.writeShellApplication {
            name = "cabal-unit-test";
            runtimeInputs = [
              pkgs.haskellPackages.hspec-discover
	      pkgs.zlib
	      pkgs.cabal-install
	      pkgs.haskell.compiler.ghc925
	      pkgs.pkg-config
            ];
            text = ''
            export PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig"
	    cabal update
	    cabal test natskell:unit-test --enable-tests --test-show-details=direct
	    '';
          };
        in {
          type = "app";
          program = "${cabal-unit-test}/bin/cabal-unit-test";
        };

        apps.cabal-sys-test = let
          cabal-sys-test = pkgs.writeShellApplication {
            name = "cabal-sys-test";
            runtimeInputs = [
              pkgs.haskellPackages.hspec-discover
	      pkgs.zlib
	      pkgs.cabal-install
	      pkgs.haskell.compiler.ghc925
	      pkgs.pkg-config
            ];
            text = ''
            export PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig"
	    cabal test natskell:system-test --enable-tests --test-show-details=direct
	    '';
          };
        in {
          type = "app";
          program = "${cabal-sys-test}/bin/cabal-sys-test";
        };

        apps.cabal-check = let
          cabal-check = pkgs.writeShellApplication {
            name = "cabal-check";
            runtimeInputs = [
	      pkgs.zlib
	      pkgs.cabal-install
	      pkgs.pkg-config
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

        apps.repl = let
          repl = pkgs.writeShellApplication {
            name = "repl";
            runtimeInputs = [
	      pkgs.zlib
	      pkgs.cabal-install
	      pkgs.pkg-config
	      pkgs.ghcid
            ];
            text = ''
            export PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig"
	    ghcid -c "cabal repl natskell"
	    '';
          };
        in {
          type = "app";
          program = "${repl}/bin/repl";
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

        apps.fmt = let
          fmt = pkgs.writeShellApplication {
            name = "fmt";
            runtimeInputs = [
              pkgs.stylish-haskell
            ];
            text = ''
	    stylish-haskell -ri -c stylish.yaml .
	    '';
          };
        in {
          type = "app";
          program = "${fmt}/bin/fmt";
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
	    pkgs.tmux
            pkgs.ormolu
          ];
          NIX_PATH = "nixpkgs=" + pkgs.path;
          PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig";
        };
      });
}
