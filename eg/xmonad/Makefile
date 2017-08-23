XMONAD=https://github.com/xmonad/xmonad
#XMONAD=$(HOME)/prj/xmonad

all: xmonad/dist/build/extrapolate/extrapolate

bench: all
	/usr/bin/time "-f%e %M" ./xmonad/dist/build/extrapolate/extrapolate

xmonad/tests/extrapolate.hs: extrapolate.hs
	cp $< $@

xmonad/dist/build/extrapolate/extrapolate: xmonad/tests/extrapolate.hs
	cd xmonad; cabal build extrapolate

prepare:
	git clone $(XMONAD) xmonad
	git -C xmonad show -s --format="%H%n%ci" | tee revision.txt
	cat xmonad.cabal >> xmonad/xmonad.cabal
	sed -i -e 's,\(removeFromWorkspace.*filter\) (/=w),\1 (==w),' xmonad/src/XMonad/StackSet.hs
	cd xmonad; cabal configure --enable-tests --ghc-options=-dynamic

clean:

clean-prepare:
	rm -rf xmonad
