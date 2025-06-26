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

        src = ./.;
        drv = hsPkgs.callCabal2nix "natskell" src {};

        patched = pkgs.haskell.lib.overrideCabal drv (old: {
          # This ensures all the test deps are in the nix store, but aren't run (as system tests are not supported in this environment)
          configureFlags = old.configureFlags or [] ++ [ "--enable-tests" ];
          doCheck = true;
          checkPhase = ''
            echo "Skipping test execution as system tests are not supported in this environment"
          '';
        });

        buildEnv = hsPkgs.shellFor {
          packages = p: [ drv ];
          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.ghc
            hsPkgs.hspec-discover
            hsPkgs.stylish-haskell
            hsPkgs.hlint
          ];
        };

        devEnv = hsPkgs.shellFor {
          packages = p: [ patched ];
          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.ghc
            hsPkgs.hspec-discover
            hsPkgs.stylish-haskell
            hsPkgs.hlint
            hsPkgs.haskell-language-server
            pkgs.docker
            pkgs.pkg-config
            pkgs.zlib.dev
            pkgs.nodejs_22 # because copilot
          ];
        };

        withCabalEnv = cmd: ''
          # don't allow cabal to resolve remote packages
          export CABAL_CONFIG=/dev/null
          # don't allow cabal to attempt writes in a sandboxed environment
          export HOME=$TMPDIR
          export XDG_CONFIG_HOME=$TMPDIR

          # move to a directory where we can write
          mkdir -p $TMPDIR/test
          cp -rT ${src} $TMPDIR/test
          cd $TMPDIR/test

          ${cmd}

          touch $out
        '';

        unitTest = pkgs.runCommand "unit-test" {
          inherit (buildEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv ''cabal v2-test natskell:test:unit-test --only'');

        fuzzTest = pkgs.runCommand "fuzz-test" {
          inherit (buildEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv ''cabal v2-test natskell:test:fuzz-test --only'');

        cabalCheck = pkgs.runCommand "cabal-check" {
          inherit (buildEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv "cabal check");

        lintCheck = pkgs.runCommand "lint-check" {
          inherit (buildEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv "find . -name \"*.hs\" -print0 | xargs -0 hlint");

        fmtCheck = pkgs.runCommand "fmt-check" {
          inherit (buildEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv "stylish-haskell -r -c stylish.yaml .");

        sdist = pkgs.runCommand "sdist" {
          inherit (buildEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv ''
          cabal sdist
          mkdir -p $out
          cp -v dist-newstyle/sdist/*.tar.gz $out/
        '');

      in {
        packages.default = sdist;

        devShells.default = devEnv;

        checks = {
          cabal-check = cabalCheck;
          unit-test = unitTest;
          fuzz-test = fuzzTest;
          lint-check = lintCheck;
          fmt-check = fmtCheck;
        };
      });
}

