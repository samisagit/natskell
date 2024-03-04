help:
	@echo "make help"
	@echo "    Show this help"
	@echo "make build"
	@echo "    Build the project"
	@echo "make test"
	@echo "    Run all tests"
	@echo "make test-all"
	@echo "    Run all tests for all resolvers"
	@echo "make unit-test"
	@echo "    Run unit tests"
	@echo "make generated-unit-test"
	@echo "    Run generated unit tests"
	@echo "make fuzz-test"
	@echo "    Run fuzz tests"
	@echo "make system-test"
	@echo "    Run system tests"
	@echo "make lint"
	@echo "    Run linter"
	@echo "make clean"
	@echo "    Clean up temporary files"
	@echo "make haddock"
	@echo "    Generate haddock documentation"
	@echo "make push-cachix"
	@echo "    Push to cachix"

haddock:
	cabal haddock natskell --open

unit-test: /tmp/unit-test.out

/tmp/unit-test.out: $(wildcard internal/*) $(wildcard client/*) $(wildcard test/Unit/*)
	cabal test natskell:unit-test --test-show-details=direct
	touch /tmp/unit-test.out

unit-test-profile: /tmp/unit-test-profile.out

/tmp/unit-test-profile.out: $(wildcard internal/*) $(wildcard client/*) $(wildcard test/Unit/*)
	cabal test natskell:unit-test --test-show-details=direct
	touch /tmp/unit-test-profile.out

fuzz-test: /tmp/fuzz-test.out

/tmp/fuzz-test.out: $(wildcard internal/*) $(wildcard client/*) $(wildcard test/Fuzz/*)
	cabal test natskell:fuzz-test --test-show-details=direct
	touch /tmp/fuzz-test.out

system-test: /tmp/system-test.out

/tmp/system-test.out: $(wildcard internal/*) $(wildcard client/*) $(wildcard test/System/*) 
	cabal test natskell:system-test --test-show-details=direct
	touch /tmp/system-test.out

test: /tmp/unit-test.out /tmp/fuzz-test.out /tmp/system-test.out

lint: /tmp/lint.out

/tmp/lint.out: $(wildcard ./**/*)
	stylish-haskell -r -c stylish.yaml .
	hlint --git
	touch /tmp/lint.out

format: /tmp/format.out

/tmp/format.out: $(wildcard ./**/*)
	stylish-haskell -r -c stylish.yaml -i . || true
	touch /tmp/format.out

PHONY clean:
clean:
	rm -f /tmp/generated-unit-test.out
	rm -f /tmp/unit-test.out
	rm -f /tmp/unit-test-profile.out
	rm -f /tmp/system-test.out
	rm -f /tmp/fuzz-test.out
	rm -f /tmp/lint.out
	rm -f /tmp/format.out

build:
	cabal build

push-cachix:
	nix develop --profile dev-profile
	cachix push samisagit-natskell dev-profile
