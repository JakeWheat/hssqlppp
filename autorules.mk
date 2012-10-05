hssqlppp/src/Database/HsSqlPpp/Annotation.o \
hssqlppp/src/Database/HsSqlPpp/Annotation.hi : hssqlppp/src/Database/HsSqlPpp/Annotation.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/src/Database/HsSqlPpp/Ast.o \
hssqlppp/src/Database/HsSqlPpp/Ast.hi : hssqlppp/src/Database/HsSqlPpp/Ast.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/src/Database/HsSqlPpp/Catalog.o \
hssqlppp/src/Database/HsSqlPpp/Catalog.hi : hssqlppp/src/Database/HsSqlPpp/Catalog.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hi : hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hs \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.hi \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package base -package groom -package mtl -package syb -package text -package uniplate -c $<

hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi : hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package base -package containers -package text -c $<

hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.hi : hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package base -package groom -package uniplate -c $<

hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.hi : hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.hi : hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package base -package text -package uniplate -c $<

hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.hi : hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package text -c $<

hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.hi : hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package base -package text -c $<

hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.hi : hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package base -package text -c $<

hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.hi : hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.hi \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package text -c $<

hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.hi : hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.lhs
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package base -package text -package uniplate -c $<

hssqlppp/src/Database/HsSqlPpp/Parser.o \
hssqlppp/src/Database/HsSqlPpp/Parser.hi : hssqlppp/src/Database/HsSqlPpp/Parser.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.hi : hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.hi \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.hi \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package base -package mtl -package parsec -package text -c $<

hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.hi : hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.lhs
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package parsec -package text -c $<

hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.hi : hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.hi \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.hi \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.hi \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package base -package mtl -package parsec -package text -package uniplate -c $<

hssqlppp/src/Database/HsSqlPpp/Pretty.o \
hssqlppp/src/Database/HsSqlPpp/Pretty.hi : hssqlppp/src/Database/HsSqlPpp/Pretty.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package pretty -package text -c $<

hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
hssqlppp/src/Database/HsSqlPpp/SqlDialect.hi : hssqlppp/src/Database/HsSqlPpp/SqlDialect.lhs
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
hssqlppp/src/Database/HsSqlPpp/TypeChecker.hi : hssqlppp/src/Database/HsSqlPpp/TypeChecker.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/src/Database/HsSqlPpp/Types.o \
hssqlppp/src/Database/HsSqlPpp/Types.hi : hssqlppp/src/Database/HsSqlPpp/Types.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/src/Database/HsSqlPpp/Utility.o \
hssqlppp/src/Database/HsSqlPpp/Utility.hi : hssqlppp/src/Database/HsSqlPpp/Utility.lhs \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.hi \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package base -package text -package uniplate -c $<

hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
hssqlppp/src/Database/HsSqlPpp/Utils/Utils.hi : hssqlppp/src/Database/HsSqlPpp/Utils/Utils.lhs
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package base -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.hi \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package text -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CreateTable.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CreateTable.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CreateTable.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Dml.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Dml.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Dml.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Joins.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Joins.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Joins.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Misc.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Misc.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Misc.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscDdl.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscDdl.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscDdl.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ParserTests.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ParserTests.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ParserTests.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/src/Database/HsSqlPpp/Pretty.hi \
    hssqlppp/src/Database/HsSqlPpp/Utility.hi \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SelectLists.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/TableRefs.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Joins.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Dml.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Misc.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CreateTable.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscDdl.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Plpgsql.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SqlServer.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package HUnit -package syb -package test-framework -package test-framework-hunit -package text -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Plpgsql.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Plpgsql.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Plpgsql.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.hi \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SelectLists.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SelectLists.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SelectLists.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SqlServer.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SqlServer.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SqlServer.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/TableRefs.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/TableRefs.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/TableRefs.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package text -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi \
    hssqlppp/src/Database/HsSqlPpp/Pretty.hi \
    hssqlppp/src/Database/HsSqlPpp/Utility.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package HUnit -package base -package test-framework -package test-framework-hunit -package text -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TestUtils.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TestUtils.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TestUtils.lhs \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package HUnit -package base -package uniplate -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/Tests.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/Tests.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/Tests.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ParserTests.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package test-framework -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TpchData.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TpchData.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TpchData.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    hssqlppp/src/Database/HsSqlPpp/Types.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package text -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Issues.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Issues.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Issues.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Joins.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Joins.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Joins.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    hssqlppp/src/Database/HsSqlPpp/Types.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    hssqlppp/src/Database/HsSqlPpp/Types.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    hssqlppp/src/Database/HsSqlPpp/Types.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TSQL.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TSQL.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TSQL.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    hssqlppp/src/Database/HsSqlPpp/Types.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Tpch.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Tpch.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Tpch.lhs \
    hssqlppp/src/Database/HsSqlPpp/Types.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TpchData.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package text -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    hssqlppp/src/Database/HsSqlPpp/Types.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Joins.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Tpch.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TSQL.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Issues.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package test-framework -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi \
    hssqlppp/src/Database/HsSqlPpp/Types.hi \
    hssqlppp/src/Database/HsSqlPpp/Pretty.hi \
    hssqlppp/src/Database/HsSqlPpp/Utility.hi \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package HUnit -package base -package test-framework -package test-framework-hunit -package text -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/junk/ExtensionTests.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/junk/ExtensionTests.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/junk/ExtensionTests.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package HUnit -package test-framework -package test-framework-hunit -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/junk/LexicalSyntaxTests.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/junk/LexicalSyntaxTests.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/junk/LexicalSyntaxTests.lhs
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/junk/PrecisionTests.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/junk/PrecisionTests.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/junk/PrecisionTests.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package HUnit -package test-framework -package test-framework-hunit -package uniplate -c $<

hssqlppp/tests/Database/HsSqlPpp/Tests/junk/RoundtripTests.o \
hssqlppp/tests/Database/HsSqlPpp/Tests/junk/RoundtripTests.hi : hssqlppp/tests/Database/HsSqlPpp/Tests/junk/RoundtripTests.lhs \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.hi \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.hi \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package HUnit -package mtl -package syb -package test-framework -package test-framework-hunit -package uniplate -c $<

hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.o \
hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.hi : hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.lhs
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package haskell-src-exts -package uniplate -c $<

hssqlppp/tests/Database/HsSqlPpp/Utils/Here.o \
hssqlppp/tests/Database/HsSqlPpp/Utils/Here.hi : hssqlppp/tests/Database/HsSqlPpp/Utils/Here.lhs
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package template-haskell -c $<

hssqlppp/tests/Tests.o \
hssqlppp/tests/Tests.hi : hssqlppp/tests/Tests.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Tests.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package test-framework -c $<

hssqlppp-pg/src/Database/HsSqlPpp/Utils/CatalogReader.o \
hssqlppp-pg/src/Database/HsSqlPpp/Utils/CatalogReader.hi : hssqlppp-pg/src/Database/HsSqlPpp/Utils/CatalogReader.lhs \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/Here2.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/PgUtils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package split -package text -c $<

hssqlppp-pg/src/Database/HsSqlPpp/Utils/Here2.o \
hssqlppp-pg/src/Database/HsSqlPpp/Utils/Here2.hi : hssqlppp-pg/src/Database/HsSqlPpp/Utils/Here2.lhs
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package template-haskell -c $<

hssqlppp-pg/src/Database/HsSqlPpp/Utils/PgUtils.o \
hssqlppp-pg/src/Database/HsSqlPpp/Utils/PgUtils.hi : hssqlppp-pg/src/Database/HsSqlPpp/Utils/PgUtils.lhs
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package HDBC -package HDBC-postgresql -package base -package text -c $<

build-src/MakeDefaultTemplate1Catalog.o \
build-src/MakeDefaultTemplate1Catalog.hi : build-src/MakeDefaultTemplate1Catalog.lhs \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.hi \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/CatalogReader.hi \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/PgUtils.hi \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package groom -package text -c $<

build-src/PostprocessUuagc.o \
build-src/PostprocessUuagc.hi : build-src/PostprocessUuagc.lhs \
    build-src/UUAGCHaddocks.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

build-src/UUAGCHaddocks.o \
build-src/UUAGCHaddocks.hi : build-src/UUAGCHaddocks.lhs
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package directory -c $<

examples/Experimentation.o \
examples/Experimentation.hi : examples/Experimentation.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package groom -package text -c $<

examples/FixSqlServerTpchSyntax.o \
examples/FixSqlServerTpchSyntax.hi : examples/FixSqlServerTpchSyntax.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/src/Database/HsSqlPpp/Pretty.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package base -package uniplate -c $<

examples/Lex.o \
examples/Lex.hi : examples/Lex.lhs \
    hssqlppp/src/Database/HsSqlPpp/Utility.hi \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package groom -package text -c $<

examples/MakeSelect.o \
examples/MakeSelect.hi : examples/MakeSelect.lhs \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/src/Database/HsSqlPpp/Pretty.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package text -c $<

examples/PPPTest.o \
examples/PPPTest.hi : examples/PPPTest.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/src/Database/HsSqlPpp/Pretty.hi \
    hssqlppp/src/Database/HsSqlPpp/Utility.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package Diff -package base -package groom -package text -package uniplate -c $<

examples/Parse.o \
examples/Parse.hi : examples/Parse.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package text -c $<

examples/Parse2.o \
examples/Parse2.hi : examples/Parse2.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package groom -package text -c $<

examples/Parse3.o \
examples/Parse3.hi : examples/Parse3.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package groom -package text -c $<

examples/QQ.o \
examples/QQ.hi : examples/QQ.lhs \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi \
    hssqlppp/src/Database/HsSqlPpp/Pretty.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base  -c $<

examples/ShowCatalog.o \
examples/ShowCatalog.hi : examples/ShowCatalog.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package groom -c $<

examples/TypeCheck.o \
examples/TypeCheck.hi : examples/TypeCheck.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi \
    hssqlppp/src/Database/HsSqlPpp/Types.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package text -c $<

examples/TypeCheck2.o \
examples/TypeCheck2.hi : examples/TypeCheck2.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi \
    hssqlppp/src/Database/HsSqlPpp/Types.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/src/Database/HsSqlPpp/Utility.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package text -c $<

examples/TypeCheck3.o \
examples/TypeCheck3.hi : examples/TypeCheck3.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.hi \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TpchData.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package text -c $<

examples/TypeCheckDB.o \
examples/TypeCheckDB.hi : examples/TypeCheckDB.lhs \
    hssqlppp/src/Database/HsSqlPpp/Parser.hi \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.hi \
    hssqlppp/src/Database/HsSqlPpp/Catalog.hi \
    hssqlppp/src/Database/HsSqlPpp/Types.hi \
    hssqlppp/src/Database/HsSqlPpp/Annotation.hi \
    hssqlppp/src/Database/HsSqlPpp/Ast.hi \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/CatalogReader.hi
	$(HC) $(HC_OPTS) -hide-all-packages -package base -package text -c $<
hssqlppp/tests/Database/HsSqlPpp/Tests/Tests : hssqlppp/tests/Database/HsSqlPpp/Tests/Tests.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Pretty.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CreateTable.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Dml.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Joins.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Misc.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscDdl.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ParserTests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Plpgsql.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SelectLists.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SqlServer.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/TableRefs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TpchData.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Issues.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Joins.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TSQL.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Tpch.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.o \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
    hssqlppp/src/Database/HsSqlPpp/Types.o \
    hssqlppp/src/Database/HsSqlPpp/Utility.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o
	$(HL) $(HL_OPTS) $(TESTS_EXTRA) \
    -o hssqlppp/tests/Database/HsSqlPpp/Tests/Tests \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Tests.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Pretty.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CreateTable.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Dml.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Joins.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Misc.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscDdl.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ParserTests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Plpgsql.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SelectLists.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SqlServer.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/TableRefs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TpchData.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Issues.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Joins.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TSQL.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Tpch.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.o \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
    hssqlppp/src/Database/HsSqlPpp/Types.o \
    hssqlppp/src/Database/HsSqlPpp/Utility.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package HUnit \
    -package base \
    -package containers \
    -package groom \
    -package haskell-src-exts \
    -package mtl \
    -package parsec \
    -package pretty \
    -package syb \
    -package template-haskell \
    -package test-framework \
    -package test-framework-hunit \
    -package text \
    -package uniplate

hssqlppp/tests/Tests : hssqlppp/tests/Tests.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Pretty.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CreateTable.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Dml.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Joins.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Misc.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscDdl.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ParserTests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Plpgsql.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SelectLists.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SqlServer.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/TableRefs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Tests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TpchData.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Issues.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Joins.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TSQL.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Tpch.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.o \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
    hssqlppp/src/Database/HsSqlPpp/Types.o \
    hssqlppp/src/Database/HsSqlPpp/Utility.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o
	$(HL) $(HL_OPTS) $(TESTS_EXTRA) \
    -o hssqlppp/tests/Tests \
    hssqlppp/tests/Tests.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Pretty.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CreateTable.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Dml.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Joins.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Misc.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscDdl.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ParserTests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Plpgsql.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SelectLists.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SqlServer.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/TableRefs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/Tests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TpchData.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Issues.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Joins.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TSQL.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Tpch.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.o \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
    hssqlppp/src/Database/HsSqlPpp/Types.o \
    hssqlppp/src/Database/HsSqlPpp/Utility.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package HUnit \
    -package base \
    -package containers \
    -package groom \
    -package haskell-src-exts \
    -package mtl \
    -package parsec \
    -package pretty \
    -package syb \
    -package template-haskell \
    -package test-framework \
    -package test-framework-hunit \
    -package text \
    -package uniplate

build-src/MakeDefaultTemplate1Catalog : build-src/MakeDefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/CatalogReader.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.o \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/Here2.o \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/PgUtils.o
	$(HL) $(HL_OPTS) $(MAKEDEFAULTTEMPLATE1CATALOG_EXTRA) \
    -o build-src/MakeDefaultTemplate1Catalog \
    build-src/MakeDefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/CatalogReader.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.o \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/Here2.o \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/PgUtils.o \
    -hide-all-packages \
    -package base \
    -package HDBC \
    -package HDBC-postgresql \
    -package base \
    -package containers \
    -package groom \
    -package split \
    -package template-haskell \
    -package text \
    -package uniplate

examples/Lex : examples/Lex.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/Utility.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o
	$(HL) $(HL_OPTS) $(LEX_EXTRA) \
    -o examples/Lex \
    examples/Lex.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/Utility.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package base \
    -package containers \
    -package groom \
    -package mtl \
    -package parsec \
    -package syb \
    -package text \
    -package uniplate

examples/MakeSelect : examples/MakeSelect.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Pretty.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o
	$(HL) $(HL_OPTS) $(MAKESELECT_EXTRA) \
    -o examples/MakeSelect \
    examples/MakeSelect.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Pretty.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package base \
    -package containers \
    -package groom \
    -package mtl \
    -package pretty \
    -package syb \
    -package text \
    -package uniplate

examples/PPPTest : examples/PPPTest.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Pretty.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/Utility.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o
	$(HL) $(HL_OPTS) $(PPPTEST_EXTRA) \
    -o examples/PPPTest \
    examples/PPPTest.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Pretty.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/Utility.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package Diff \
    -package base \
    -package containers \
    -package groom \
    -package mtl \
    -package parsec \
    -package pretty \
    -package syb \
    -package text \
    -package uniplate

examples/Parse : examples/Parse.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o
	$(HL) $(HL_OPTS) $(PARSE_EXTRA) \
    -o examples/Parse \
    examples/Parse.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package base \
    -package containers \
    -package groom \
    -package mtl \
    -package parsec \
    -package syb \
    -package text \
    -package uniplate

examples/Parse2 : examples/Parse2.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o
	$(HL) $(HL_OPTS) $(PARSE2_EXTRA) \
    -o examples/Parse2 \
    examples/Parse2.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package base \
    -package containers \
    -package groom \
    -package haskell-src-exts \
    -package mtl \
    -package parsec \
    -package syb \
    -package text \
    -package uniplate

examples/Parse3 : examples/Parse3.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o
	$(HL) $(HL_OPTS) $(PARSE3_EXTRA) \
    -o examples/Parse3 \
    examples/Parse3.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package base \
    -package containers \
    -package groom \
    -package haskell-src-exts \
    -package mtl \
    -package parsec \
    -package syb \
    -package text \
    -package uniplate

examples/TypeCheck : examples/TypeCheck.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
    hssqlppp/src/Database/HsSqlPpp/Types.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o
	$(HL) $(HL_OPTS) $(TYPECHECK_EXTRA) \
    -o examples/TypeCheck \
    examples/TypeCheck.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
    hssqlppp/src/Database/HsSqlPpp/Types.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package base \
    -package containers \
    -package groom \
    -package mtl \
    -package parsec \
    -package syb \
    -package text \
    -package uniplate

examples/TypeCheck2 : examples/TypeCheck2.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
    hssqlppp/src/Database/HsSqlPpp/Types.o \
    hssqlppp/src/Database/HsSqlPpp/Utility.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o
	$(HL) $(HL_OPTS) $(TYPECHECK2_EXTRA) \
    -o examples/TypeCheck2 \
    examples/TypeCheck2.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
    hssqlppp/src/Database/HsSqlPpp/Types.o \
    hssqlppp/src/Database/HsSqlPpp/Utility.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package base \
    -package containers \
    -package groom \
    -package mtl \
    -package parsec \
    -package syb \
    -package text \
    -package uniplate

examples/TypeCheck3 : examples/TypeCheck3.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TpchData.o \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o
	$(HL) $(HL_OPTS) $(TYPECHECK3_EXTRA) \
    -o examples/TypeCheck3 \
    examples/TypeCheck3.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/tests/Database/HsSqlPpp/Tests/TpchData.o \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.o \
    hssqlppp/tests/Database/HsSqlPpp/Utils/Here.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package base \
    -package containers \
    -package groom \
    -package haskell-src-exts \
    -package mtl \
    -package parsec \
    -package syb \
    -package template-haskell \
    -package text \
    -package uniplate

examples/TypeCheckDB : examples/TypeCheckDB.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
    hssqlppp/src/Database/HsSqlPpp/Types.o \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/CatalogReader.o \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/Here2.o \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/PgUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o
	$(HL) $(HL_OPTS) $(TYPECHECKDB_EXTRA) \
    -o examples/TypeCheckDB \
    examples/TypeCheckDB.o \
    hssqlppp/src/Database/HsSqlPpp/Annotation.o \
    hssqlppp/src/Database/HsSqlPpp/Ast.o \
    hssqlppp/src/Database/HsSqlPpp/Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.o \
    hssqlppp/src/Database/HsSqlPpp/Parser.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.o \
    hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.o \
    hssqlppp/src/Database/HsSqlPpp/SqlDialect.o \
    hssqlppp/src/Database/HsSqlPpp/TypeChecker.o \
    hssqlppp/src/Database/HsSqlPpp/Types.o \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/CatalogReader.o \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/Here2.o \
    hssqlppp-pg/src/Database/HsSqlPpp/Utils/PgUtils.o \
    hssqlppp/src/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package HDBC \
    -package HDBC-postgresql \
    -package base \
    -package containers \
    -package groom \
    -package mtl \
    -package parsec \
    -package split \
    -package syb \
    -package template-haskell \
    -package text \
    -package uniplate
