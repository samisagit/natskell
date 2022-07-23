.PHONY: coverage
coverage:
	stack test --system-ghc --coverage
	xdg-open `stack path --local-hpc-root`/index.html

unit-test.out: $(wildcard src/*) $(wildcard test/Unit/*)
	stack test  natskell:unit-test --system-ghc --ta '-j 16 --format=failed-examples'
	touch unit-test.out

fuzz-test.out: $(wildcard src/*) $(wildcard test/Fuzz/*)
	stack test  natskell:fuzz-test --system-ghc --ta '-j 16 --format=failed-examples'
	touch fuzz-test.out

system-test.out: $(wildcard src/*) $(wildcard test/System/*) 
	docker pull nats:latest
	stack test natskell:system-test --system-ghc --ta '-j 16 --format=failed-examples'
	touch system-test.out

test: unit-test.out fuzz-test.out system-test.out

lint.out: $(wildcard src/*) $(wildcard test/*) 
	hlint --git
	touch lint.out

build-test:
	stack test --fast --system-ghc --no-run-tests --ta '-j 16'

build:
	stack build --fast --system-ghc -j 16
