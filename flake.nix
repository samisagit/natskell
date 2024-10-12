{
  description = "A flake to develop Natskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
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
        devShell = pkgs.mkShell{ 
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

	apps.default = let
    	  build = pkgs.writeShellApplication {
    	    name = "build";
    	    runtimeInputs = [pkgs.cabal-install pkgs.haskell.compiler.ghc925];
    	    text = ''
	      cabal update
	      cabal build natskell
    	    '';
    	  };
    	in {
    	  type = "app";
    	  program = "${build}/bin/build";
    	};

	apps.test = let
    	  test = pkgs.writeShellApplication {
    	    name = "test";
    	    runtimeInputs = [pkgs.cabal-install pkgs.haskell.compiler.ghc925 pkgs.haskellPackages.hspec-discover pkgs.zlib];
    	    text = ''
	      cabal update
	      cabal test --test-show-details=direct
    	    '';
    	  };
    	in {
    	  type = "app";
    	  program = "${test}/bin/test";
    	};

	apps.lint = let
    	  lint = pkgs.writeShellApplication {
    	    name = "lint";
    	    runtimeInputs = [pkgs.ghc pkgs.haskellPackages.hlint pkgs.haskellPackages.stylish-haskell];
    	    text = ''
	      stylish-haskell -r -c stylish.yaml .
	      hlint --git
	      cabal check
    	    '';
    	  };
    	in {
    	  type = "app";
    	  program = "${lint}/bin/lint";
    	};
      });
}
