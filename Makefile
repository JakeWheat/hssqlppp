AG_FILES = $(shell find src -iname '*ag')

# make the website
# devel tool needs fixing
#website : src-extra/tosort/util/DevelTool
#	src-extra/tosort/util/DevelTool makewebsite +RTS -N

# make the haddock and put in the correct place in the generated
# website
website_haddock :
	cabal configure
	cabal haddock
	-mkdir hssqlppp
	mv dist/doc/html/hssqlppp hssqlppp/haddock

# task to build the chaos sql, which takes the source sql
# transforms it and produces something which postgres understands
CHAOS_SQL_SRC = $(shell find src-extra/chaos/sql/ -iname '*.sql')

chaos.sql : $(CHAOS_SQL_SRC) dist/build/h7c/h7c
	dist/build/h7c/h7c > chaos.sql

dist/build/h7c/h7c :
	echo "please use cabal configure -fdevutils and cabal build"

# this needs an old version of uuagc such as 0.9.39.1
# which doesn't work with the latest ghc for some reason
# TODO: instructions on how to make this work
src/Database/HsSqlPpp/Internals/AstInternal.hs : $(AG_FILES)
	uuagc -dcfwsp -P src/Database/HsSqlPpp/Internals/ \
		src/Database/HsSqlPpp/Internals/AstInternal.ag

# rule for the generated file
# src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs
# don't want to automatically keep this up to date, only regenerate it
# manually

regenDefaultTemplate1Catalog : dist/build/MakeDefaultTemplate1Catalog/MakeDefaultTemplate1Catalog
	dist/build/MakeDefaultTemplate1Catalog/MakeDefaultTemplate1Catalog > \
		src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs_new
	mv src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs_new \
		src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs

# todo: make sure all generated files are either under dist or build
# and simplify this
.PHONY : clean
clean :
	-rm -Rf dist
	find . -iname '*.o' -delete
	find . -iname '*.hi' -delete
	-rm chaos.sql
	-rm -Rf hssqlppp

maintainer-clean : clean
	-rm src/Database/HsSqlPpp/Internals/AstInternal.hs
