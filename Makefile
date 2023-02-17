init:
	cabal update

generated-unit-test: /tmp/generated-unit-test.out

/tmp/generated-unit-test.out: $(wildcard src/*) $(wildcard test/Unit/*)
	cabal test  natskell:unit-test
	touch /tmp/generated-unit-test.out

unit-test: /tmp/unit-test.out

/tmp/unit-test.out: $(wildcard src/*) $(wildcard test/Unit/*)
	cabal test  natskell:unit-test
	touch /tmp/unit-test.out

fuzz-test: /tmp/fuzz-test.out

/tmp/fuzz-test.out: $(wildcard src/*) $(wildcard test/Fuzz/*)
	cabal test  natskell:fuzz-test
	touch /tmp/fuzz-test.out

system-test: /tmp/system-test.out

/tmp/system-test.out: $(wildcard src/*) $(wildcard test/System/*) 
	cabal test natskell:system-test
	touch /tmp/system-test.out

test: /tmp/unit-test.out /tmp/fuzz-test.out /tmp/system-test.out

lint: /tmp/lint.out

/tmp/lint.out: $(wildcard src/*) $(wildcard test/*) 
	hlint --git
	touch /tmp/lint.out

PHONY clean:
clean:
	rm -f /tmp/generated-unit-test.out
	rm -f /tmp/unit-test.out
	rm -f /tmp/system-test.out
	rm -f /tmp/fuzz-test.out
	rm -f /tmp/lint.out

build:
	cabal build

push-cachix:
	nix develop --profile dev-profile
	cachix push samisagit-natskell dev-profile
