build: hpack
	cabal --ghc-options='${GHC_OPTIONS}' build

hpack:
	hpack .

test: hpack
	cabal --ghc-options='${GHC_OPTIONS}' test

format-haskell:
	find src/ test/ -name "*.hs" -exec fourmolu -i {} +

format-nix:
	alejandra --quiet .

format: format-nix format-haskell

ghcid: hpack
	ghcid -c "cabal --ghc-options='${GHC_OPTIONS}' repl"

hlint: hpack
	hlint .

# Add version bounds automatically
bounds: hpack
	cabal gen-bounds

# Mostly documentation for future uploads
hackage: hpack
	cabal new-haddock --haddock-for-hackage derive-has-field
	cabal new-sdist
	# cabal upload --publish dist-newstyle/sdist/derive-has-field-0.1.0.0.tar.gz
	# cabal upload --publish -d dist-newstyle/derive-has-field-0.1.0.0-docs.tar.gz

.PHONY: build hpack test run format-haskell format-nix format ghcid hlint bounds hackage
