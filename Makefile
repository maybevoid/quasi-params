
clean:
	cabal clean

build:
	cabal build --enable-tests --enable-benchmark --enable-documentation all

test:
	cabal run quasi-params-test

.PHONY: clean build test
