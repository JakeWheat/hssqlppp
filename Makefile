
# quick start, use:
#
# make sandbox
# make test
#
# this will build the library and run the tests
#
# to build everything
#
# make sandbox-devel
# make really-all


# works with ghc 7.10.1 on 110415
# make sandbox && make test
# but make sandbox-all doesn't work
# you don't need this just to work on hssqlppp

# everything should work with ghc 7.6.3 and 7.8.4

##############################################################################

# cabal sandbox stuff

.PHONY : sandbox-init
sandbox-init :
	cabal sandbox init --sandbox sandbox
	cd hssqlppp; cabal sandbox init --sandbox ../sandbox/
	cd hssqlppp-th; cabal sandbox init --sandbox ../sandbox/
	cd hssqlppp-pg; cabal sandbox init --sandbox ../sandbox/
	cd examples; cabal sandbox init --sandbox ../sandbox/
	cd postprocess-uuagc; cabal sandbox init --sandbox ../sandbox/
	cd build-extras; cabal sandbox init --sandbox ../sandbox/
	cd make-website && cabal sandbox init --sandbox ../sandbox/

# the default sandbox installs teh minimum needed to build hssqlppp
# and run the tests

.PHONY : sandbox
sandbox : sandbox-init sandbox/bin/happy
	cabal install hssqlppp/ hssqlppp-th/ examples/ postprocess-uuagc/ \
  --only-dependencies --enable-tests -j

sandbox/bin/happy : sandbox-init
	cabal install happy -j

# install all the extra stuff for developing hssqlppp

# this adds the postgres support to the sandbox, you need the
# postgresql c client dev package(s) installed to use this

.PHONY : sandbox-devel
sandbox-devel : sandbox-init sandbox/bin/happy
	cabal install hssqlppp/ hssqlppp-th/ examples/ postprocess-uuagc/ \
  build-extras/ hssqlppp-pg/ make-website/ \
  --only-dependencies --enable-tests -j

##############################################################################

# the default target
# the main use of this target is to check everything still compiles
# successfully

.PHONY : all
.DEFAULT : all
all : hssqlppp hssqlppp-th examples test

# the test target can be used when working on hssqlppp and also
# doesn't need postgres client drivers

.PHONY : test
test : hssqlppp hssqlppp-th
	hssqlppp/dist/build/Tests/Tests --hide-successes
	hssqlppp-th/dist/build/TestsTh/TestsTh --hide-successes

# if you need some other combination of targets please add them to the
# makefile

# this does everything, make sure you've done all the sandbox targets
.PHONY : really-all
really-all : test examples hssqlppp-pg \
  sandbox/bin/MakeDefaultTemplate1Catalog \
  website website-haddock


##############################################################################

# building the components

# the project is separated into several separate cabal projects. Some
# of these are intended to end up on hackage: hssqlppp, hssqlppp-th
# and hssqlppp-pg, and the others are just for developing hssqlppp
# itself

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
examples : hssqlppp hssqlppp-th
	cd examples && cabal configure && cabal build -j

sandbox/bin/PostprocessUuagc :
	cd postprocess-uuagc && cabal install -j

sandbox/bin/MakeDefaultTemplate1Catalog : hssqlppp-pg
	cd build-extras && cabal install -j

# todo: get the dependencies right for these build commands
# this means just the .ag compilation and the rest is deferred to
# cabal
# maybe this should change because cabal is unbelievably slow when there is
# no work to do
# also: better dependencies might speed things up a bit, I don't know
# if I am using it completely wrong, but cabal seems to produce a
# really poor amount of parallelism in the builds making them very
# slow, and it seems to take absolutely ages on null builds which
# shows when you want to edit one file, then recompile and run the
# tests

# also: for the binaries created by the local packages, should the
# sandbox installed binaries be used, or the ones under the respective
# dist/ directories? I'm not sure what the right place is.


##############################################################################

# rebuilding the website. You probably won't need to use this

.PHONY : make-website/dist/build/MakeWebsite/MakeWebsite
make-website/dist/build/MakeWebsite/MakeWebsite :
	cd make-website && cabal build -j

# make the website
.PHONY : website
website : #make-website/dist/build/MakeWebsite/MakeWebsite \
  #build/website/main.css
	mkdir -p build/website
	asciidoctor website-source/index.asciidoc -o - | runhaskell website-source/AddLinks.lhs > build/website/index.html
	asciidoctor website-source/examples.asciidoc -o - | runhaskell website-source/AddLinks.lhs > build/website/examples.html
	#make-website/dist/build/MakeWebsite/MakeWebsite
	#asciidoctor build/website/ParserTests.asciidoc -o build/website/ParserTests.html
	#asciidoctor build/website/TypeCheckTests.asciidoc -o build/website/TypeCheckTests.html
	#asciidoctor build/website/QuasiQuoteTests.asciidoc -o build/website/QuasiQuoteTests.html

	#mv build/website/index.txt.html build/website/index.html

DIAGRAM_SRC_FILES=$(shell ls hssqlppp/src/Database/HsSqlPpp/*.lhs)

build/website/hssqlppp-src.svg : $(DIAGRAM_SRC_FILES)
	mkdir -p build/website
	graphmod -i ~/wd/hssqlppp/master/hssqlppp/src $(DIAGRAM_SRC_FILES) \
	    -a -R Text -R Control -R Data -r Prelude \
	    > build/website/hssqlppp-src.dot
	dot -Tsvg build/website/hssqlppp-src.dot -o build/website/hssqlppp-src.svg

# build/website/main.css : website-source/main.css
# 	-mkdir -p build/website/
# 	cp website-source/main.css build/website/main.css

# make the haddock and put in the correct place in the generated
# website

# to build the full website you need the 'website' and the
# 'website-haddock' targets. These are kept separate so that when
# editing the website and rebuilding to check, you don't need to keep
# redoing the haddock as well which slows down the cycle.

.PHONY : website-haddock
website-haddock :
	cd hssqlppp && cabal haddock
	-mkdir -p build/website
	-rm -Rf build/website/haddock
	cp -R hssqlppp/dist/doc/html/hssqlppp build/website/haddock

##############################################################################

# building the tarballs for hackage. Maybe there should be more
# support here, for automated checking and uploading, to document the
# process and make it more difficult to miss a step?

.PHONY : sdists
sdists :
	cd hssqlppp; cabal sdist
	cd hssqlppp-th; cabal sdist
	cd hssqlppp-pg; cabal sdist

.PHONY : check-sdists
check-sdists : sdists
	-rm -Rf /tmp/hssqlppp*
	cd hssqlppp; sh ~/.cabal/share/cabal-scripts-0.1/cabal-test \
          dist/hssqlppp-0.5.16.tar.gz
	cd hssqlppp-th; sh ~/.cabal/share/cabal-scripts-0.1/cabal-test \
          dist/hssqlppp-th-0.5.16.tar.gz
	cd hssqlppp-pg; sh ~/.cabal/share/cabal-scripts-0.1/cabal-test \
          dist/hssqlppp-pg-0.5.16.tar.gz

##############################################################################

# generated files

# hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hs
# generated from the .ag files under hssqlppp/src/Database/HsSqlPpp/Internals/

# specific rules for generated file astinternal.hs
AG_FILES = $(shell find hssqlppp/src -iname '*ag')

# the dependency on build-src/PostprocessUuagc.lhs isn't quite right
# want to depend on build-src/PostprocessUuagc.lhs and all its
# other dependencies as source files as well
# don't want to depend on the exe since this causes build problems
# with cyclic dependencies and with rebuilding stuff too often

hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hs : $(AG_FILES) \
  sandbox/bin/PostprocessUuagc
	sandbox/bin/uuagc -dcfspwm \
	        -P hssqlppp/src/Database/HsSqlPpp/Internals/ \
		--lckeywords --doublecolons --genlinepragmas \
		hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.ag
	sandbox/bin/PostprocessUuagc
	# prefix all the generated stuff with _
	# so we don't get loads of unused function warnings
	sed -r -i -e "s/([a-zA-Z0-9_]*_(Inh|Syn)_[a-zA-Z0-9_]*)/_\1/g" -e "s/((sem|wrap)_[a-zA-Z0-9_]*)/_\1/g" hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hs
#	sed -r -i -e "s/\b([^ ]*_(Inh|Syn)_[^ ]*)\b/_\1/" -e "s/\b((Inh|Syn|sem|wrap)_[^ ]*)/_\1/" hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hs


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

##############################################################################

# cleaning targets

.PHONY : clean
clean :
	cd hssqlppp && cabal clean
	cd hssqlppp-th && cabal clean
	cd hssqlppp-pg && cabal clean
	cd examples && cabal clean
	cd postprocess-uuagc && cabal clean
	cd build-extras && cabal clean
	cd make-website && cabal clean
	rm -Rf build

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
	-rm make-website/cabal.sandbox.config


.PHONY : clean-all
clean-all : clean clean-sandbox
