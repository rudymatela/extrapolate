GHC=ghc-7.8
BENCH= \
  sorting \
  calculator \
  parser \
  overflow \
  word-refinements \
  list
TYM= \
  /usr/bin/time "-f%e %M"

all: $(BENCH)

bench: all
	@- $(TYM) ./list
	@- $(TYM) ./sorting
	@- $(TYM) ./calculator
	@- $(TYM) ./parser
	#@- $(TYM) ./word-refinements

%: %.hs
	$(GHC) -dynamic -O2 -package-db .cabal-sandbox/*-$(GHC).*-packages.conf.d $< && touch $@

prepare:
	cabal sandbox init
	git clone https://github.com/UoYCS-plasma/LazySmallCheck2012 lazysmallcheck2012
	git -C lazysmallcheck2012 show -s --format="%H%n%ci" | tee revision.txt
	cabal sandbox add-source lazysmallcheck2012
	cabal-$(GHC) install lazysmallcheck2012

clean:
	rm -f $(BENCH) *.hi *.o

clean-prepare:
	rm -rf .cabal-sandbox cabal.sandbox.config lazysmallcheck2012
