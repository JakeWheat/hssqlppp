
# this command will prepare the shared sandbox. This is the
# recommended way of developing hssqlppp

.PHONY : sandbox
sandbox :
	cabal sandbox init --sandbox sandbox
	cd src-extra; cabal sandbox init --sandbox ../sandbox/
	cabal install uuagc-bootstrap uuagc-cabal -j
	cabal install uuagc-0.9.39.1 -j
	cabal install . src-extra/ --only-dependencies --enable-tests -j

.DEFAULT : all
all : hssqlppp sandbox/bin/TypeCheckDB sandbox/bin/TypeCheck \
  sandbox/bin/ShowCatalog sandbox/bin/QQ sandbox/bin/Parse2 \
  sandbox/bin/Parse sandbox/bin/MakeSelect \
  sandbox/bin/FixSqlServerTpchSyntax \
  sandbox/bin/MakeDefaultTemplate1Catalog \
  test


.PHONY : hssqlppp
hssqlppp : src/Database/HsSqlPpp/Internals/AstInternal.hs
	cabal configure --enable-tests && cabal build -j
	-cabal sandbox hc-pkg unregister hssqlppp -- --force
	cabal install -j


sandbox/bin/TypeCheckDB sandbox/bin/TypeCheck \
  sandbox/bin/ShowCatalog sandbox/bin/QQ sandbox/bin/Parse2 \
  sandbox/bin/Parse sandbox/bin/MakeSelect \
  sandbox/bin/FixSqlServerTpchSyntax \
  sandbox/bin/MakeDefaultTemplate1Catalog :
	cd src-extra && cabal configure && \
          cabal build -j && cabal install -j

.PHONY : test
test : hssqlppp
	dist/build/Tests/Tests --hide-successes

AG_FILES = $(shell find src -iname '*ag')

src/Database/HsSqlPpp/Internals/AstInternal.hs : $(AG_FILES)
	sandbox/bin/uuagc -dcfspwm -P src/Database/HsSqlPpp/Internals/ \
		--lckeywords --doublecolons --genlinepragmas \
		src/Database/HsSqlPpp/Internals/AstInternal.ag


.PHONY : regenDefaultTemplate1Catalog
regenDefaultTemplate1Catalog : sandbox/bin/MakeDefaultTemplate1Catalog
	sandbox/bin/MakeDefaultTemplate1Catalog > \
		src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs_new
	mv src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs_new \
		src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs


.PHONY : clean
clean :
	cabal clean
	cd src-extra && cabal clean
	-rm chaos.sql

.PHONY : clean-sandbox
clean-sandbox :
	-rm -Rf sandbox
	-rm cabal.sandbox.config
	-rm src-extra/cabal.sandbox.config

.PHONY : clean-all
clean-all : clean clean-sandbox


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
#CHAOS_SQL_SRC = $(shell find src-extra/chaos/sql/ -iname '*.sql')

#chaos.sql : $(CHAOS_SQL_SRC) sandbox/bin/h7c
#	sandbox/bin/h7c > chaos.sql
