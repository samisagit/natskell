.PHONY: coverage
coverage:
	cabal test all --enable-coverage || true
	mkdir report -p
	mv `find . -type f -name "*.html" | grep /hpc/` report || true
	xdg-open `find report -type f -name "hpc_index.html"`

.PHONY: test
test:
	cabal test all
