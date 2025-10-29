format:
	ormolu --mode inplace $$(find . -name '*.hs')
	cabal-gild --io=sifaka2/sifaka2.cabal
	cabal-gild --io=fnotation/fnotation.cabal
