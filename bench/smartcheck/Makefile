GHC=ghc
GHCI=ghci
BENCH= \
  sorting \
  parser \
  list \
  overflow \
  word-refinements \
  div0
TYM= \
  /usr/bin/time "-f%e %M"
SMARTCHECK=https://github.com/leepike/SmartCheck
#SMARTCHECK=$(HOME)/prj/smartcheck

all: $(BENCH)

bench: all
	@- yes | $(TYM) ./div0
	@- yes | $(TYM) ./parser
	@- yes | $(TYM) ./sorting
	@- yes | $(TYM) ./list
	@- yes | $(TYM) ./overflow
	@- yes | $(TYM) ./word-refinements

bench-sorting: sorting
	mkdir -p $<-out
	for i in {00..99}; do \
	  yes | \
	  /usr/bin/time "-f%e %M" ./$< > $<-out/$$i; \
	done 2> $<.runtime

bench-sorting-swap: sorting
	mkdir -p $<-swap-out
	for i in {00..99}; do \
	  yes | \
	  /usr/bin/time "-f%e %M" ./$< swap > $<-swap-out/$$i; \
	done 2> $<-swap.runtime

bench-sorting-uncurry: sorting
	mkdir -p $<-uncurry-out
	for i in {000..999}; do \
	  yes | \
	  /usr/bin/time "-f%e %M" ./$< uncurry | \
	  grep -A2 -E "(^forall values|OK, passed)" > $<-uncurry-out/$$i; \
	done 2> $<-uncurry.runtime

bench-parser: parser
	mkdir -p $<-out
	for i in {000..999}; do \
	  yes | \
	  /usr/bin/time "-f%e %M" ./$< | \
	  grep "^Lang" > $<-out/$$i; \
	done 2> $<.runtime

bench-overflow: overflow
	mkdir -p $<-out
	for i in {000..999}; do \
	  yes | \
	  /usr/bin/time "-f%e %M" ./$< | \
	  grep -A2 "^forall values" > $<-out/$$i; \
	done 2> $<.runtime

bench-div0: div0
	mkdir -p div0-out
	for i in {000..999}; do yes | /usr/bin/time "-f%e %M" ./div0 | grep -A3 forall > div0-out/$$i; done 2> div0.runtime

bench-word-refinements: word-refinements
	mkdir -p $<-out
	for i in {000..999}; do \
	  yes | /usr/bin/time "-f%e %M" ./$< | \
	  grep -A2 -E "(^forall values|OK, passed)" > $<-out/$$i; \
	done 2> $<.runtime

div0.hs: div0-main.hs smartcheck/regression/Div0/Div0.hs
	cat smartcheck/regression/Div0/Div0.hs div0-main.hs > div0.hs

parser.hs: parser-main.hs smartcheck/regression/Parser/Parser.hs
	cat smartcheck/regression/Parser/Parser.hs parser-main.hs > parser.hs

overflow.hs: overflow-main.hs smartcheck/regression/PaperExample1/PaperExample1.hs
	cat smartcheck/regression/PaperExample1/PaperExample1.hs overflow-main.hs > overflow.hs

overflow: overflow.hs

%: %.hs
	$(GHC) -dynamic -O2 -ignore-package QuickCheck-2.10.1 -package-db .cabal-sandbox/*-$(GHC)*-packages.conf.d $< && touch $@

%.ghci: %.hs
	$(GHCI) -dynamic -package-db .cabal-sandbox/*-$(GHC)*-packages.conf.d $<

prepare:
	git clone $(SMARTCHECK) smartcheck
	git -C smartcheck show -s --format="%H%n%ci" | tee revision.txt
	cabal sandbox init
	cabal sandbox add-source smartcheck
	cabal install smartcheck

clean:
	rm -f $(BENCH) *.hi *.o div0.hs parser.hs overflow.hs

clean-prepare:
	rm -rf .cabal-sandbox cabal.sandbox.config smartcheck
