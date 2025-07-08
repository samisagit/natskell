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
          packages = p: [ drv ];
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
        } (withCabalEnv ''cabal test natskell:test:unit-test --only --test-show-details=failures'');

        fuzzTest = pkgs.runCommand "fuzz-test" {
          inherit (buildEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv ''cabal test natskell:test:fuzz-test --only --test-show-details=failures'');

        # TODO: this doesn't work, the docker sock is missing
        sysTest = pkgs.runCommand "sys-test" {
          inherit (devEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv ''cabal test natskell:test:system-test --only --test-show-details=direct -fimpure'');

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

        haddock = pkgs.runCommand "haddock" {
          inherit (buildEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv ''
          cabal haddock client/Client.hs
          mkdir -p $out
          cp -vr $(dirname $(find dist-newstyle -type f -path "*/doc/html/*/index.html" | grep '/doc/html/natskell/index.html$')) $out/
        '');

      in {
        packages.sdist = sdist;
        packages.docs = haddock;
	packages.default = drv;
	packages.system-test = sysTest;

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

