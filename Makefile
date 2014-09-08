
# this command will prepare the shared sandbox. This is the
# recommended way of developing hssqlppp

.PHONY : sandbox
sandbox :
	cabal sandbox init --sandbox sandbox
	cd hssqlppp; cabal sandbox init --sandbox ../sandbox/
	cd hssqlppp-th; cabal sandbox init --sandbox ../sandbox/
	cd hssqlppp-pg; cabal sandbox init --sandbox ../sandbox/
	cd examples; cabal sandbox init --sandbox ../sandbox/
	cd postprocess-uuagc; cabal sandbox init --sandbox ../sandbox/
	cd build-extras; cabal sandbox init --sandbox ../sandbox/
	cabal install uuagc-bootstrap uuagc-cabal
	cabal install uuagc-0.9.39.1
	cabal install hssqlppp/ hssqlppp-th/ hssqlppp-pg/ examples/ postprocess-uuagc/ build-extras/ --only-dependencies --enable-tests -j

# TODO: make the sandbox optional and add option to change location of
# sandbox

# TODO: support using another packagedb not via sandboxes or something

.PHONY : all
.DEFAULT : all
all : sandbox/bin/PostprocessUuagc hssqlppp hssqlppp-th \
  hssqlppp-pg sandbox/bin/MakeDefaultTemplate1Catalog \
  examples test

.PHONY : test
test : hssqlppp hssqlppp-th
	hssqlppp/dist/build/Tests/Tests --hide-successes
	hssqlppp-th/dist/build/TestsTh/TestsTh --hide-successes

# todo: get the dependencies right for these build commands
# this means just the .ag compilation and the rest is deferred to
# cabal
# maybe this should change is cabal is unbelievably slow when there is
# no work to do

.PHONY : hssqlppp
hssqlppp : hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hs
	cd hssqlppp && cabal configure --enable-tests && cabal build -j
	-cabal sandbox hc-pkg unregister hssqlppp-pg -- --force
	-cabal sandbox hc-pkg unregister hssqlppp-th -- --force
	-cabal sandbox hc-pkg unregister hssqlppp -- --force
	cd hssqlppp && cabal install -j

.PHONY : hssqlppp-th
hssqlppp-th : hssqlppp
	cd hssqlppp-th && cabal configure --enable-tests && cabal build -j
	-cabal sandbox hc-pkg unregister hssqlppp-pg -- --force
	-cabal sandbox hc-pkg unregister hssqlppp-th -- --force
	cd hssqlppp-th && cabal install -j

.PHONY : hssqlppp-pg
hssqlppp-pg : hssqlppp-th
	cd hssqlppp-pg && cabal configure --enable-tests && cabal build -j
	-cabal sandbox hc-pkg unregister hssqlppp-pg -- --force
	cd hssqlppp-pg && cabal install -j

.PHONY : examples
examples : hssqlppp hssqlppp-th hssqlppp-pg
	cd examples && cabal configure && cabal build -j

sandbox/bin/PostprocessUuagc :
	cd postprocess-uuagc && cabal install -j

sandbox/bin/MakeDefaultTemplate1Catalog : hssqlppp-pg
	cd build-extras && cabal install -j

# make the website
# the devel-tool is currently broken, it needs fixing first
#website : src-extra/docutil/DevelTool
#	src-extra/docutil/DevelTool makewebsite +RTS -N

# make the haddock and put in the correct place in the generated
# website
#website-haddock :
#	cabal configure
#	cabal haddock
#	-mkdir hssqlppp
#	-rm -Rf hssqlppp/haddock
#	mv dist/doc/html/hssqlppp hssqlppp/haddock

.PHONY : sdists
sdists :
	cd hssqlppp; cabal sdist
	cd hssqlppp-th; cabal sdist
	cd hssqlppp-pg; cabal sdist

.PHONY : check-sdists
check-sdists : sdists
	-rm -Rf /tmp/hssqlppp*
	cd hssqlppp; sh ~/.cabal/share/cabal-scripts-0.1/cabal-test dist/hssqlppp-0.5.10.tar.gz
	cd hssqlppp-th; sh ~/.cabal/share/cabal-scripts-0.1/cabal-test dist/hssqlppp-th-0.5.10.tar.gz
	cd hssqlppp-pg; sh ~/.cabal/share/cabal-scripts-0.1/cabal-test dist/hssqlppp-pg-0.5.10.tar.gz

# specific rules for generated file astinternal.hs
# the latest version of uuagc which I know works is 0.9.39.1
# if you get errors like this:
# error: Undefined local variable or field ...
# then try downgrading your version of uuagc (or fix the .ag code!)
AG_FILES = $(shell find hssqlppp/src -iname '*ag')

# the dependency on build-src/PostprocessUuagc.lhs isn't quite right
# want to depend on build-src/PostprocessUuagc.lhs and all its
# other dependencies as source files as well
# don't want to depend on the exe since this causes build problems
# with cyclic dependencies and with rebuilding stuff too often

hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hs : $(AG_FILES) sandbox/bin/PostprocessUuagc
	sandbox/bin/uuagc -dcfspwm -P hssqlppp/src/Database/HsSqlPpp/Internals/ \
		--lckeywords --doublecolons --genlinepragmas \
		hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.ag
	sandbox/bin/PostprocessUuagc


#-dcfspwm --cycle -O
# rule for the generated file
# src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs
# don't want to automatically keep this up to date, only regenerate it
# manually

.PHONY : regenDefaultTemplate1Catalog
regenDefaultTemplate1Catalog : sandbox/bin/MakeDefaultTemplate1Catalog
	sandbox/bin/MakeDefaultTemplate1Catalog > \
		hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs_new
	mv hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs_new \
		hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs


.PHONY : clean
clean :
	cd hssqlppp && cabal clean
	cd hssqlppp-th && cabal clean
	cd hssqlppp-pg && cabal clean
	cd examples && cabal clean
	cd postprocess-uuagc && cabal clean
	cd build-extras && cabal clean

.PHONY : clean-sandbox
clean-sandbox :
	-rm -Rf sandbox
	-rm cabal.sandbox.config
	-rm hssqlppp/cabal.sandbox.config
	-rm -Rf hssqlppp/.cabal-sandbox/
	-rm hssqlppp-th/cabal.sandbox.config
	-rm -Rf hssqlppp-th/.cabal-sandbox/
	-rm hssqlppp-pg/cabal.sandbox.config
	-rm -Rf hssqlppp-pg/.cabal-sandbox/
	-rm -Rf examples/cabal.sandbox.config
	-rm -Rf examples/.cabal-sandbox
	-rm -Rf postprocess-uuagc/cabal.sandbox.config
	-rm -Rf postprocess-uuagc/.cabal-sandbox
	-rm -Rf build-extras/cabal.sandbox.config
	-rm -Rf build-extras/.cabal-sandbox


.PHONY : clean-all
clean-all : clean clean-sandbox
