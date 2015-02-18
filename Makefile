
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
  --only-dependencies --enable-tests -j --constraint "Cabal==1.18.1.3"

sandbox/bin/happy : sandbox-init
	cabal install happy -j

# install all the extra stuff for developing hssqlppp

# this adds the postgres support to the sandbox, you need the
# postgresql c client dev package(s) installed to use this

.PHONY : sandbox-devel
sandbox-devel : sandbox-init sandbox/bin/happy
	cabal install hssqlppp/ hssqlppp-th/ examples/ postprocess-uuagc/ \
  build-extras/ hssqlppp-pg/ make-website/ \
  --only-dependencies --enable-tests -j --constraint "Cabal==1.18.1.3"

# if you get an error like this:

# uuagc-0.9.39.1 failed during the configure step. The exception was:
# user error (
# /tmp/uuagc-0.9.39.1-31295/uuagc-0.9.39.1/dist/dist-sandbox-4d052f7a/setup/setup.hs:8:30:
# Couldn't match expected type ‘UserHooks’
# with actual type ‘Cabal-1.18.1.3:Distribution.Simple.UserHooks.UserHooks’
# NB: ‘UserHooks’
# is defined in ‘Distribution.Simple.UserHooks’
# in package ‘Cabal-1.20.0.0’
# ‘Cabal-1.18.1.3:Distribution.Simple.UserHooks.UserHooks’
# is defined in ‘Distribution.Simple.UserHooks’
# in package ‘Cabal-1.18.1.3’
# In the first argument of ‘defaultMainWithHooks’, namely
# ‘(uuagcUserHook' compiler)’
# In the expression: defaultMainWithHooks (uuagcUserHook' compiler)
# )

# the work around is to unregister all the versions of Cabal in your
# user package database except version 1.18.1.3
# this could break some things, but you can reinstall the versions you
# unregister after uuagc is compiled
# (this is a bug in cabal-install/ Cabal, which is only an issue
# because we are using a really old version of uuagc)

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
really-all : test hssqlppp-pg sandbox/bin/MakeDefaultTemplate1Catalog \
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
website : make-website/dist/build/MakeWebsite/MakeWebsite \
  build/website/main.css
	make-website/dist/build/MakeWebsite/MakeWebsite
	mv build/website/index.txt.html build/website/index.html

build/website/main.css : website-source/main.css
	-mkdir -p build/website/
	cp website-source/main.css build/website/main.css

# make the haddock and put in the correct place in the generated
# website

# to build the full website you need the 'website' and the
# 'website-haddock' targets. These are kept separate so that when
# editing the website and rebuilding to check, you don't need to keep
# redoing the haddock as well which slows down the cycle.

.PHONY : website-haddock
website-haddock :
	cd hssqlppp && cabal configure && cabal haddock
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

hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hs : $(AG_FILES) \
  sandbox/bin/PostprocessUuagc
	sandbox/bin/uuagc -dcfspwm -P \
		hssqlppp/src/Database/HsSqlPpp/Internals/ \
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
