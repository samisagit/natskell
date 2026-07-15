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
        src = ./.;
        hsPkgs = pkgs.haskellPackages.override {
          overrides = self: super: {
            natskell = self.callCabal2nix "natskell" src {};
            natskell-system-tests = self.callCabal2nix "natskell-system-tests" ./system-tests {};
          };
        };

        drv = hsPkgs.natskell;
        sysTestDrv = hsPkgs.natskell-system-tests;

        buildEnv = hsPkgs.shellFor {
          packages = p: [ drv sysTestDrv ];
          nativeBuildInputs = [
            hsPkgs.cabal-install
            hsPkgs.ghc
            hsPkgs.hspec-discover
            hsPkgs.stylish-haskell
            hsPkgs.hlint
          ];
        };

        devEnv = hsPkgs.shellFor {
          packages = p: [ drv sysTestDrv ];
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
            pkgs.nsc
            pkgs.natscli
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

        publicApiTest = pkgs.runCommand "public-api-test" {
          inherit (buildEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv ''cabal test natskell:test:public-api-test --only --test-show-details=failures'');

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

        hackageDocs = pkgs.runCommand "hackage-docs" {
          inherit (buildEnv) buildInputs nativeBuildInputs;
        } (withCabalEnv ''
          package="$(awk '/^name:/ { print $2; exit }' natskell.cabal)"
          version="$(awk '/^version:/ { print $2; exit }' natskell.cabal)"
          docs_root="$package-$version-docs"
          combined_docs="$TMPDIR/$docs_root"

          cabal haddock --haddock-for-hackage "$package"
          main_docs_index="$(find dist-newstyle -type f -path "*/doc/html/$docs_root/API.html" -print -quit)"
          if [ -z "$main_docs_index" ]; then
            echo "Could not find default library Hackage docs"
            find dist-newstyle -type f -path "*/doc/html/*" -print
            exit 1
          fi
          cp -R "$(dirname "$main_docs_index")" "$combined_docs"

          mkdir -p $out
          tar --format=ustar -C "$TMPDIR" -czf "$out/$docs_root.tar.gz" "$docs_root"
        '');

      in {
        packages.sdist = sdist;
        packages.docs = haddock;
        packages.hackage-docs = hackageDocs;
        packages.default = drv;

        devShells.default = devEnv;

        checks = {
          cabal-check = cabalCheck;
          unit-test = unitTest;
          fuzz-test = fuzzTest;
          public-api-test = publicApiTest;
          lint-check = lintCheck;
          fmt-check = fmtCheck;
        };
      });
}
