HC = ~/Documents/haskell/ghc/_build/stage1/bin/ghc
HC_TMP = compdir
HC_PACKAGE_DB = /home/munin/Documents/haskell/ghc/_build/stage1/lib/package.conf.d

HC_FLAGS = -odir=$(HC_TMP) -hidir=$(HC_TMP)
HC_PACKAGE_SPEC = -hide-all-packages -no-user-package-db -package-db=$(HC_PACKAGE_DB)
HC_PACKAGES = -package=ghc -package=base -package=containers
HC_EXTS = -XLambdaCase
HSFILES = comp.hs

comp: 	comp.hs
	$(HC) $(HC_PACKAGE_SPEC) $(HC_PACKAGES) $(HC_EXTS) $(HC_FLAGS) $(HSFILES) -o Comp

all:	comp

clean:
	rm -rf $(HC_TMP) ./Comp output ./A

.PHONY: clean all
