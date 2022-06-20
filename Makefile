.PHONY: coverage
coverage:
	stack test --system-ghc --coverage
	xdg-open `stack path --local-hpc-root`/index.html

.PHONY: unit-test
unit-test:
	stack test  natskell:unit-test --system-ghc --ta '-j 16' 

.PHONY: fuzz-test
fuzz-test:
	stack test  natskell:fuzz-test --system-ghc --ta '-j 16' 

.PHONY: system-test
system-test:
	docker pull nats:latest
	stack test natskell:system-test --system-ghc --ta '-j 16'

.PHONY: test
test:
	docker pull nats:latest
	stack test --system-ghc --ta '-j 16'

.PHONY: build-test
build-test:
	stack test --system-ghc --no-run-tests --ta '-j 16'

.PHONY: build
build:
	stack build --system-ghc -j 16
