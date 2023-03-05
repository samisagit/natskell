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
	stack haddock natskell --open

generated-unit-test: /tmp/generated-unit-test.out

/tmp/generated-unit-test.out: $(wildcard src/*) $(wildcard test/Unit/*)
	stack test natskell:unit-test --fast --cabal-verbosity=silent --ta="-j 16 --format=failed-examples --fail-fast --match=generated"
	touch /tmp/generated-unit-test.out

unit-test: /tmp/unit-test.out

/tmp/unit-test.out: $(wildcard src/*) $(wildcard test/Unit/*)
	stack test natskell:unit-test --fast --cabal-verbosity=silent --ta="-j 16 --format=progress --fail-fast --skip=generated"
	touch /tmp/unit-test.out

fuzz-test: /tmp/fuzz-test.out

/tmp/fuzz-test.out: $(wildcard src/*) $(wildcard test/Fuzz/*)
	stack test  natskell:fuzz-test --fast --cabal-verbosity=silent --ta="-j 16 --format=progress --fail-fast"
	touch /tmp/fuzz-test.out

system-test: /tmp/system-test.out

/tmp/system-test.out: $(wildcard src/*) $(wildcard test/System/*) 
	stack test natskell:system-test --fast --cabal-verbosity=silent --ta="-j 16 --format=progress --fail-fast"
	touch /tmp/system-test.out

test: /tmp/unit-test.out /tmp/fuzz-test.out /tmp/system-test.out

lint: /tmp/lint.out

/tmp/lint.out: $(wildcard src/*) $(wildcard test/*) 
	hlint --git
	touch /tmp/lint.out

test-all: ./*stack.yaml
	for file in $^; do \
		if ! stack --stack-yaml $${file} test --fast --cabal-verbosity=silent --ta="-j 16 --format=failed-examples --fail-fast" ; then \
			echo "Test failed for resolver $${file}"; \
			exit 1; \
		fi; \
	done

PHONY clean:
clean:
	rm -f /tmp/generated-unit-test.out
	rm -f /tmp/unit-test.out
	rm -f /tmp/system-test.out
	rm -f /tmp/fuzz-test.out
	rm -f /tmp/lint.out

build:
	stack build --fast

build-test:
	stack test --fast --no-run-tests --ta '-j 16'

push-cachix:
	nix develop --profile dev-profile
	cachix push samisagit-natskell dev-profile
