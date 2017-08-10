# Makefile for Extrapolate
#
# Copyright:   (c) 2015-2017 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
TESTS = \
  tests/test-derive \
  tests/test-utils \
  tests/test-extrapolate
QUICKEG = \
  eg/int \
  eg/list \
  eg/calculator \
  eg/gencalc \
  eg/parser \
  eg/heap \
  eg/redblack \
  eg/sorting
EG = \
  bench/true  \
  bench/false \
  bench/eq    \
  bench/diff  \
  bench/ord   \
  eg/overflow \
  eg/overflow8 \
  $(QUICKEG)
LISTHS   = find src mk tests eg bench/*.hs -name \*.hs
LISTOBJS = $(LISTHS) | sed -e 's/.hs$$/.o/'
LISTLIBS = find src -name \*.hs
ALLHS    = $(shell $(LISTHS))
HSS      = $(shell $(LISTHS))
ALLOBJS  = $(shell $(LISTOBJS))
OBJS = src/Test/Extrapolate.o
GHCIMPORTDIRS = src:tests:eg
GHCEXTRAFLAGS =
GHCFLAGS = -dynamic -O2 $(GHCEXTRAFLAGS)
HADDOCKFLAGS = --no-print-missing-docs

all: $(OBJS)

all-all: $(ALLOBJS)

test: $(patsubst %,%.test,$(TESTS)) diff-test

diff-test: $(patsubst %,%.diff-test,$(EG))

quick-test: $(patsubst %,%.test,$(TESTS)) quick-diff-test

quick-diff-test: $(patsubst %,%.diff-test,$(QUICKEG))

update-diff-test: $(patsubst %,%.update-diff-test,$(EG))

%.test: %
	./$<

%.diff-test: %
	./$< | diff -rud tests/model/$< -

%.update-diff-test: %
	./$< >           tests/model/$<

test-sdist:
	./tests/test-sdist

bench: $(patsubst %,%.bench,$(EG))

.PHONY: %.bench
%.bench: %
	@mkdir -p `dirname bench/runtime/$$HOSTNAME/$<`
	@printf "%-18s " $<
	@/usr/bin/time -f%e ./$< 2>&1 >/dev/null | tee bench/runtime/$$HOSTNAME/$<

clean: clean-hi-o clean-haddock
	rm -f $(TESTS) $(EG) mk/toplibs

ghci: mk/All.ghci

install:
	@echo "use \`cabal install' instead"

list-hs:
	$(LISTHS)

list-objs:
	$(LISTOBJS)

legacy-test: # needs ghc-7.10 and ghc-7.8 installed as such
	make clean && make test -j8 GHC=ghc-7.10 GHCFLAGS="-Werror -dynamic"
	make clean && make test -j8 GHC=ghc-7.8  GHCFLAGS="-Werror -dynamic"
	make clean && make test

legacy-test-via-cabal: # needs similarly named cabal wrappers
	cabal clean && cabal-ghc-7.10 configure --ghc-option=-dynamic && cabal-ghc-7.10 test
	cabal clean && cabal-ghc-7.8  configure --ghc-option=-dynamic && cabal-ghc-7.8  test
	cabal clean && cabal test

prepare-legacy-test: prepare-legacy-test-7.10 prepare-legacy-test-7.8

prepare-legacy-test-7.10:
	cabal-ghc-7.10 --ignore-sandbox install leancheck speculate

prepare-legacy-test-7.8:
	cabal-ghc-7.8  --ignore-sandbox install leancheck speculate

hlint:
	hlint \
	  --ignore "Use import/export shortcut" \
	  --ignore "Redundant bracket" \
	  .

markdown:
	pandoc README.md -o README.html

haddock: doc/index.html

clean-haddock:
	rm -f doc/*.{html,css,js,png,gif} README.html

upload-haddock:
	@echo "use \`cabal upload -d' instead"
	@echo "(but 1st: cabal install --only-dependencies --enable-documentation)"
	@echo "(to just compile docs: cabal haddock --for-hackage)"

doc/index.html: $(shell $(LISTLIBS))
	./mk/haddock-i base template-haskell | xargs \
	haddock --html -odoc $(shell $(LISTLIBS)) $(HADDOCKFLAGS) --title=extrapolate

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and tests programs so long as they don't share dependencies _not_ stored
# in src/ and tests/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

include mk/haskell.mk
