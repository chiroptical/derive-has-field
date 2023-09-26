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

.PHONY: build hpack test run format-haskell format-nix format ghcid hlint
