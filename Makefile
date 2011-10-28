
# the default make target is the automated tests exe

# todo: get all the sh files and builds and scripts and put them all
# in here instead

# stuff to add:
# regenDefaultTemplate1Catalog.sh
# build the lib using cabal
# do stuff with chaos:
#   build the 'compiler' exe only
#   produce the transformed sql
#   load the sql into pg
#   output stuff: typecheck, documentation#
# clean
# already have:
#   recompile ag
#   makeantinodes
# compile all the examples to make sure they work
#   (maybe alter the docs to capture the output for these?)
# compile and run tests
# make the website -> this can also do the haddock for the website
# h7c stuff: needs some thought
# get the chaos sql transformation documentation working again

# want to avoid editing source or the commands to try different things
# in development
# how can have better control over conditionally compiling in different
#   test modules?

#todo: find something better than make

#this makefile is probably written wrong

# simple todos: check regen catalog
#  add make website command
#  do the new dependency stuff

CC = please dont use this implicit rule

HC      = ghc
HC_OPTS = -Wall -XTupleSections -XScopedTypeVariables -XDeriveDataTypeable  -threaded -rtsopts -isrc:src-extra/util:src-extra/tests/:src-extra/devel-util:src-extra/chaos:src-extra/chaos/extensions:src-extra/examples:src-extra/h7c

EXE_FILES = src-extra/tests/Tests \
	src-extra/devel-util/MakeAntiNodesRunner \
	src-extra/devel-util/MakeDefaultTemplate1Catalog \
	src-extra/examples/MakeSelect \
	src-extra/examples/Parse \
	src-extra/examples/Parse2 \
	src-extra/examples/QQ \
	src-extra/examples/ShowCatalog \
	src-extra/examples/TypeCheck \
	src-extra/examples/TypeCheckDB

#	src-extra/examples/FixSqlServerTpchSyntax
#	src-extra/chaos/build.lhs

# used for dependency generation
# first all the modules which are public in the cabal package
# this is all the hs files directly in the src/Database/HsSqlPpp/ folder
# then it lists the lhs for all the exe files
SRCS_ROOTS = $(shell find src/Database/HsSqlPpp/ -maxdepth 1 -iname '*hs') \
	     $(EXE_FILES)

AG_FILES = $(shell find src -iname '*ag')

# -include exe_rules.mk

# doesn't work: generated instead
#$(EXE_FILES) : $(addsuffix .o,$@)
#	$(HC) $(HC_OPTS) $(addsuffix .lhs,$@)

src-extra/tests/Tests : src-extra/tests/Tests.o
	$(HC) $(HC_OPTS) $(addsuffix .lhs,$@)

src-extra/devel-util/MakeAntiNodesRunner : src-extra/devel-util/MakeAntiNodesRunner.o
	$(HC) $(HC_OPTS) $(addsuffix .lhs,$@)




make_exe_deps : i_like_make.lhs Makefile
	ghc i_like_make.lhs
	./i_like_make

all : $(EXE_FILES)

%.hi : %.o
	@:

%.o : %.lhs
	$(HC) $(HC_OPTS) -c $<

%.o : %.hs
	$(HC) $(HC_OPTS) -c $<


depend :
	ghc -M $(HC_OPTS) $(SRCS_ROOTS) -dep-makefile .depend

#specific rules for generated files: astanti.hs and astinternal.hs

src/Database/HsSqlPpp/Internals/AstAnti.hs : src/Database/HsSqlPpp/Internals/AstInternal.hs src-extra/devel-util/MakeAntiNodesRunner
	src-extra/devel-util/MakeAntiNodesRunner

src/Database/HsSqlPpp/Internals/AstInternal.hs : $(AG_FILES)
	uuagc  -dcfwsp -P src/Database/HsSqlPpp/Internals/ src/Database/HsSqlPpp/Internals/AstInternal.ag

#rule for the generated file src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs
# don't want to automatically keep this up to date though, only regenerate it manually

.PHONY : regenDefaultTemplate1Catalog
regenDefaultTemplate1Catalog : src-extra/devel-util/MakeDefaultTemplate1Catalog
	src-extra/devel-util/MakeDefaultTemplate1Catalog > src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs_new
	mv src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs_new src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs

.PHONY : clean
clean :
	-rm -Rf dist
	find . -iname '*.o' -delete
	find . -iname '*.hi' -delete
	-rm $(EXE_FILES)

-include .depend
