# Makefile for Extrapolate
#
# Copyright:   (c) 2015-2019 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
TESTS = \
  test/derive \
  test/utils \
  test/step-by-step \
  test/extrapolate
QUICKEG = \
  eg/sorting \
  eg/int \
  eg/list \
  eg/calculator \
  eg/gencalc \
  eg/parser \
  eg/heap \
  eg/ratio \
  eg/redblack \
  eg/word-refinements
EG = \
  bench/true  \
  bench/false \
  bench/eq    \
  bench/diff  \
  bench/ord   \
  eg/overflow \
  eg/overflow8 \
  $(QUICKEG)
GHCIMPORTDIRS = src:test:eg
GHCEXTRAFLAGS = #-prof -fprof-auto #-caf-all
# When profiling is enabled, to get the cost centres with more than 6% time:
#   $ ./eg/sorting +RTS -p -RTS
#   $ cat sorting.prof | grep -v ' [0-5].[0-9] ......$'
GHCFLAGS = -O2 \
  $(GHCEXTRAFLAGS) \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic)
#  -Wall -Wno-name-shadowing -Wno-orphans -Wno-unused-matches
HADDOCKFLAGS = \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo --optghc=-dynamic)
LIST_ALL_HSS = find src test mk eg/*.hs bench/*.hs -name "*.hs"
LIB_DEPS = base $(INSTALL_DEPS)
INSTALL_DEPS = leancheck express speculate template-haskell transformers
all: mk/toplibs

all-all: mk/All.hs

test: $(patsubst %,%.run,$(TESTS)) diff-test test-sdist

diff-test: $(patsubst %,%.diff-test,$(EG))

quick-test: $(patsubst %,%.run,$(TESTS)) quick-diff-test test-sdist

quick-diff-test: $(patsubst %,%.diff-test,$(QUICKEG))

update-diff-test: $(patsubst %,%.update-diff-test,$(EG))

egs: $(EG)

%.run: %
	./$<

%.diff-test: %
	./$< | diff -rud test/model/$<.out -

%.update-diff-test: %
	./$< >           test/model/$<.out

test-sdist:
	./test/sdist

test-via-cabal:
	cabal configure --enable-tests --enable-benchmarks --ghc-options="$(GHCFLAGS) -O0"
	cabal build
	cabal test test

test-via-stack:
	stack test extrapolate:test:test --ghc-options="$(GHCFLAGS) -O0" --system-ghc --no-install-ghc --no-terminal

.PHONY: bench
bench: $(patsubst %,%.bench,$(EG))
	@mkdir -p bench/runtime/$$HOSTNAME
	./bench/versions | tee bench/runtime/$$HOSTNAME/versions

.PHONY: %.bench
%.bench: %
	@mkdir -p bench/runtime/$$HOSTNAME
	@printf "%-18s " $<
	@/usr/bin/time -f%e ./$< 2>&1 >/dev/null | tee bench/runtime/$$HOSTNAME/$<.runtime

clean: clean-hi-o clean-haddock
	rm -f $(TESTS) $(EG) mk/toplibs
	rm -f eg/redblack-*-bug* eg/RedBlackSet*Bug.*

ghci: mk/All.ghci

ghci-test: test/Test.ghci

ghci-7.10: GHC=ghc-7.10
ghci-7.10: mk/All.ghci

ghci-7.8: GHC=ghc-7.8
ghci-7.8: mk/All.ghci

install:
	@echo "use \`cabal install' instead"

legacy-test: # needs ghc-8.2 .. ghc-7.8 installed as such
	make clean && make test -j8 GHC=ghc-8.2  GHCFLAGS="-Werror -dynamic"
	make clean && make test -j8 GHC=ghc-8.0  GHCFLAGS="-Werror -dynamic"
	make clean && make test -j8 GHC=ghc-7.10 GHCFLAGS="-Werror -dynamic"
	make clean && make test -j8 GHC=ghc-7.8  GHCFLAGS="-Werror -dynamic"
	make clean && make test -j8

legacy-test-via-cabal: # needs similarly named cabal wrappers
	cabal clean && cabal-ghc-8.2  configure && cabal-ghc-8.2  test
	cabal clean && cabal-ghc-8.0  configure && cabal-ghc-8.0  test
	cabal clean && cabal-ghc-7.10 configure && cabal-ghc-7.10 test
	cabal clean && cabal-ghc-7.8  configure && cabal-ghc-7.8  test
	cabal clean && cabal test

prepare-legacy-test: \
  prepare-legacy-test-8.2 \
  prepare-legacy-test-8.0 \
  prepare-legacy-test-7.10 \
  prepare-legacy-test-7.8

prepare-legacy-test-8.2:
	cabal-ghc-8.2  --ignore-sandbox install leancheck speculate

prepare-legacy-test-8.0:
	cabal-ghc-8.0  --ignore-sandbox install leancheck speculate

prepare-legacy-test-7.10:
	cabal-ghc-7.10 --ignore-sandbox install leancheck speculate

prepare-legacy-test-7.8:
	cabal-ghc-7.8  --ignore-sandbox install leancheck speculate

hlint:
	hlint \
	  --ignore "Use import/export shortcut" \
	  --ignore "Redundant bracket" \
	  src eg/sorting.hs eg/list.hs eg/word-refinements.hs

markdown:
	pandoc README.md -o README.html

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and test programs so long as they don't share dependencies _not_ stored
# in src/ and test/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

include mk/haskell.mk


eg/RedBlackSetRemoveBug.hspp: eg/RedBlackSet.hs
	$(GHCCMD) -DREMOVE_BUG -E $< -o $@

eg/RedBlackSetRemoveBug.hs: eg/RedBlackSetRemoveBug.hspp
	sed -e "s/module RedBlackSet/&RemoveBug/" $< > $@

eg/redblack-remove-bug.hs: eg/redblack.hs
	sed -e "s/import RedBlackSet/&RemoveBug/" $< > $@

eg/redblack-remove-bug: eg/redblack-remove-bug.hs eg/RedBlackSetRemoveBug.hs mk/toplibs


eg/RedBlackSetDeleteBug.hspp: eg/RedBlackSet.hs
	$(GHCCMD) -DDELETE_BUG -E $< -o $@

eg/RedBlackSetDeleteBug.hs: eg/RedBlackSetDeleteBug.hspp
	sed -e "s/module RedBlackSet/&DeleteBug/" $< > $@

eg/redblack-delete-bug.hs: eg/redblack.hs
	sed -e "s/import RedBlackSet/&DeleteBug/" $< > $@

eg/redblack-delete-bug: eg/redblack-delete-bug.hs eg/RedBlackSetDeleteBug.hs mk/toplibs


eg/RedBlackSetBalanceBug.hspp: eg/RedBlackSet.hs
	$(GHCCMD) -DBALANCE_BUG -E $< -o $@

eg/RedBlackSetBalanceBug.hs: eg/RedBlackSetBalanceBug.hspp
	sed -e "s/module RedBlackSet/&BalanceBug/" $< > $@

eg/redblack-balance-bug.hs: eg/redblack.hs
	sed -e "s/import RedBlackSet/&BalanceBug/" $< > $@

eg/redblack-balance-bug: eg/redblack-balance-bug.hs eg/RedBlackSetBalanceBug.hs mk/toplibs
