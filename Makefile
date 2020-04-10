
clean:
	cabal clean

build:
	cabal build --enable-tests --enable-benchmark --enable-documentation all

.PHONY: clean build
