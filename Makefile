.PHONY: coverage
coverage:
	stack test --coverage
	xdg-open `stack path --local-hpc-root`/index.html

.PHONY: test
test:
	stack test
