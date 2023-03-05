generated-unit-test: /tmp/generated-unit-test.out

/tmp/generated-unit-test.out: $(wildcard src/*) $(wildcard test/Unit/*)
	stack test natskell:unit-test --ta="-j 16 --format=failed-examples --fail-fast --match=generated"
	touch /tmp/generated-unit-test.out

unit-test: /tmp/unit-test.out

/tmp/unit-test.out: $(wildcard src/*) $(wildcard test/Unit/*)
	stack test natskell:unit-test --ta="-j 16 --format=progress --fail-fast --skip=generated"
	touch /tmp/unit-test.out

fuzz-test: /tmp/fuzz-test.out

/tmp/fuzz-test.out: $(wildcard src/*) $(wildcard test/Fuzz/*)
	stack test  natskell:fuzz-test --ta="-j 16 --format=progress --fail-fast"
	touch /tmp/fuzz-test.out

system-test: /tmp/system-test.out

/tmp/system-test.out: $(wildcard src/*) $(wildcard test/System/*) 
	stack test natskell:system-test --ta="-j 16 --format=progress --fail-fast"
	touch /tmp/system-test.out

test: /tmp/unit-test.out /tmp/fuzz-test.out /tmp/system-test.out

lint: /tmp/lint.out

/tmp/lint.out: $(wildcard src/*) $(wildcard test/*) 
	hlint --git
	touch /tmp/lint.out

test-all: ./*stack.yaml
	for file in $^; do \
		if stack --stack-yaml $${file} test --ta="-j 16 --format=failed-examples --fail-fast" ; then \
			make clean; \
		else \
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
	stack build

push-cachix:
	nix develop --profile dev-profile
	cachix push samisagit-natskell dev-profile
