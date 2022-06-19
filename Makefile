.PHONY: coverage
coverage:
	stack test --system-ghc --coverage
	xdg-open `stack path --local-hpc-root`/index.html

.PHONY: test
test:
	stack test --system-ghc

.PHONY: build-test
build-test:
	stack test --system-ghc --no-run-tests

.PHONY: build
build:
	stack build --system-ghc
