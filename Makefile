.PHONY: coverage
coverage:
	stack test --system-ghc --coverage
	xdg-open `stack path --local-hpc-root`/index.html

.PHONY: test
test:
	stack test --system-ghc --ta '-j 16' 

.PHONY: build-test
build-test:
	stack test --system-ghc --no-run-tests --ta '-j 16'

.PHONY: build
build:
	stack build --system-ghc -j 16
