{
  description = "A flake to develop Natskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils/master";
  };

  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
        hsPkgs = pkgs.haskellPackages;

        src = builtins.path { path = ./.; name = "natskell-src"; };

        drv = hsPkgs.callCabal2nix "natskell" src {};

        testEnv = hsPkgs.shellFor {
          packages = p: [ drv ];
          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.ghc
            hsPkgs.hspec-discover
            hsPkgs.stylish-haskell
            hsPkgs.hlint
            pkgs.pkg-config
            pkgs.zlib.dev
          ];
        };

        withCabalEnv = cmd: ''
          # don't allow cabal to attempt writes in a sandboxed environment
          export CABAL_CONFIG=/dev/null
          export HOME=$TMPDIR
          export XDG_CONFIG_HOME=$TMPDIR

          # move to a directory where we can write
          mkdir -p $TMPDIR/test
          cp -rT ${src} $TMPDIR/test
          cd $TMPDIR/test

          ${cmd}

          touch $out
        '';

        testNames = [ "unit-test" "fuzz-test" ];

        testDerivations = pkgs.lib.genAttrs testNames (name:
          pkgs.runCommand name {
            inherit (testEnv) buildInputs nativeBuildInputs;
          } (withCabalEnv ''cabal v2-test natskell:test:${name} --only'')
        );

        cabalCheck = pkgs.runCommand "cabal-check" {
          inherit (testEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv "cabal check");

        lintCheck = pkgs.runCommand "lint-check" {
          inherit (testEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv "find . -name \"*.hs\" -print0 | xargs -0 hlint");

        fmtCheck = pkgs.runCommand "fmt-check" {
          inherit (testEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv "stylish-haskell -r -c stylish.yaml .");

      in {
        packages.default = drv;
        checks = {
          cabal-check = cabalCheck;
          unit-test = testDerivations."unit-test";
          fuzz-test = testDerivations."fuzz-test";
	  lint-check = lintCheck;
	  fmt-check = fmtCheck;
        };
      });
}

