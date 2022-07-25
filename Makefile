.PHONY: coverage
coverage:
	stack test --system-ghc --coverage
	xdg-open `stack path --local-hpc-root`/index.html

generated-unit-test: /tmp/generated-unit-test.out

/tmp/generated-unit-test.out: $(wildcard src/*) $(wildcard test/Unit/*)
	stack test  natskell:unit-test --system-ghc --ta '-j 16 --format=failed-examples --match=generated'
	touch /tmp/generated-unit-test.out

unit-test: /tmp/unit-test.out

/tmp/unit-test.out: $(wildcard src/*) $(wildcard test/Unit/*)
	stack test  natskell:unit-test --system-ghc --ta '-j 16 --format=failed-examples --skip=generated'
	touch /tmp/unit-test.out

fuzz-test: /tmp/fuzz-test.out

/tmp/fuzz-test.out: $(wildcard src/*) $(wildcard test/Fuzz/*)
	stack test  natskell:fuzz-test --system-ghc --ta '-j 16 --format=failed-examples'
	touch /tmp/fuzz-test.out

system-test: /tmp/system-test.out

/tmp/system-test.out: $(wildcard src/*) $(wildcard test/System/*) 
	stack test natskell:system-test --system-ghc --ta '-j 16 --format=failed-examples'
	touch /tmp/system-test.out

test: /tmp/unit-test.out /tmp/fuzz-test.out /tmp/system-test.out

lint: /tmp/lint.out

/tmp/lint.out: $(wildcard src/*) $(wildcard test/*) 
	hlint --git
	touch /tmp/lint.out

PHONY clean:
clean:
	rm -f /tmp/unit-test.out
	rm -f /tmp/system-test.out
	rm -f /tmp/fuzz-test.out
	rm -f /tmp/lint.out

build-test:
	stack test --fast --system-ghc --no-run-tests --ta '-j 16'

build:
	stack build --fast --system-ghc -j 16
