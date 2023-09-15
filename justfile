prepare:
	cabal update

clean:
	cabal clean

build: prepare
	cabal build

test: prepare
	cabal test

publish: clean prepare
	cabal v2-haddock --haddock-for-hackage ./manifest/extism-manifest.cabal
	cabal v2-haddock --haddock-for-hackage
	cabal sdist ./manifest/extism-manifest.cabal
	cabal sdist

format:
	# TODO

lint:
	# cabal check
	hlint src manifest

