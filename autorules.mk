$(BUILD)/Database/HsSqlPpp/Annotation.o : hssqlppp/src/Database/HsSqlPpp/Annotation.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Annotation.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Ast.o : hssqlppp/src/Database/HsSqlPpp/Ast.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Ast.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Catalog.o : hssqlppp/src/Database/HsSqlPpp/Catalog.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Catalog.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o : hssqlppp/src/Database/HsSqlPpp/Internals/AstInternal.hs \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Internals/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package groom -package mtl -package syb -package text -package uniplate -c $< -o $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o : hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Internals/Catalog/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package containers -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o : hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Internals/Catalog/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package groom -package uniplate -c $< -o $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o : hssqlppp/src/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Internals/Catalog/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o : hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/Environment.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -package uniplate -c $< -o $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o : hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o : hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o : hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o : hssqlppp/src/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.hi \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o : hssqlppp/src/Database/HsSqlPpp/Internals/TypesInternal.lhs
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Internals/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -package uniplate -c $< -o $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Parser.o : hssqlppp/src/Database/HsSqlPpp/Parser.lhs \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Parser.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o : hssqlppp/src/Database/HsSqlPpp/Parsing/Lexer.lhs \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package mtl -package parsec -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o : hssqlppp/src/Database/HsSqlPpp/Parsing/ParseErrors.lhs
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package parsec -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o : hssqlppp/src/Database/HsSqlPpp/Parsing/ParserInternal.lhs \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.hi \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.hi \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package mtl -package parsec -package text -package uniplate -c $< -o $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Pretty.o : hssqlppp/src/Database/HsSqlPpp/Pretty.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package pretty -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Pretty.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/SqlDialect.o : hssqlppp/src/Database/HsSqlPpp/SqlDialect.lhs
	-mkdir -p $(BUILD)/Database/HsSqlPpp/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/TypeChecker.o : hssqlppp/src/Database/HsSqlPpp/TypeChecker.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.hi \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/TypeChecker.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Types.o : hssqlppp/src/Database/HsSqlPpp/Types.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Types.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Utility.o : hssqlppp/src/Database/HsSqlPpp/Utility.lhs \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.hi \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi \
    $(BUILD)/Database/HsSqlPpp/Parser.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -package uniplate -c $< -o $(BUILD)/Database/HsSqlPpp/Utility.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Utils/Utils.o : hssqlppp/src/Database/HsSqlPpp/Utils/Utils.lhs
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Utils/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Utils/Utils.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.lhs \
    $(BUILD)/Database/HsSqlPpp/Utils/Here.hi \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/CreateTable.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/CreateTable.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/CreateTable.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/Dml.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Dml.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Dml.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/Joins.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Joins.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Joins.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/Misc.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Misc.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Misc.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/MiscDdl.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscDdl.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/MiscDdl.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/ParserTests.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ParserTests.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/Pretty.hi \
    $(BUILD)/Database/HsSqlPpp/Utility.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/SelectLists.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/TableRefs.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Joins.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Dml.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Misc.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/CreateTable.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/MiscDdl.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Plpgsql.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/SqlServer.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package HUnit -package base -package syb -package test-framework -package test-framework-hunit -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/ParserTests.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/Plpgsql.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Plpgsql.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Plpgsql.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.lhs \
    $(BUILD)/Database/HsSqlPpp/Utils/Here.hi \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/SelectLists.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SelectLists.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/SelectLists.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/SqlServer.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/SqlServer.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/SqlServer.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/TableRefs.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/TableRefs.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/TableRefs.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Parsing/Utils.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/Parsing/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/QuasiQuoteTests.o : hssqlppp/tests/Database/HsSqlPpp/Tests/QuasiQuoteTests.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi \
    $(BUILD)/Database/HsSqlPpp/Pretty.hi \
    $(BUILD)/Database/HsSqlPpp/Utility.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package HUnit -package base -package test-framework -package test-framework-hunit -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/QuasiQuoteTests.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TestUtils.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TestUtils.lhs \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package HUnit -package base -package uniplate -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TestUtils.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/Tests.o : hssqlppp/tests/Database/HsSqlPpp/Tests/Tests.lhs \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/ParserTests.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/QuasiQuoteTests.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package test-framework -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/Tests.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TpchData.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TpchData.lhs \
    $(BUILD)/Database/HsSqlPpp/Utils/Here.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TpchData.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.lhs \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/Types.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.lhs \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Issues.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Issues.lhs \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Issues.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Joins.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Joins.lhs \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/Types.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Joins.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.lhs \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.lhs \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/Types.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.lhs \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/Types.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TSQL.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TSQL.lhs \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/Types.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TSQL.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Tpch.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Tpch.lhs \
    $(BUILD)/Database/HsSqlPpp/Types.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TpchData.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Tpch.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.lhs \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/Types.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.lhs \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Joins.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Tpch.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TSQL.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Issues.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package test-framework -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.o : hssqlppp/tests/Database/HsSqlPpp/Tests/TypeChecking/Utils.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi \
    $(BUILD)/Database/HsSqlPpp/Types.hi \
    $(BUILD)/Database/HsSqlPpp/Pretty.hi \
    $(BUILD)/Database/HsSqlPpp/Utility.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package HUnit -package base -package test-framework -package test-framework-hunit -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/junk/ExtensionTests.o : hssqlppp/tests/Database/HsSqlPpp/Tests/junk/ExtensionTests.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi \
    $(BUILD)/Database/HsSqlPpp/Ast.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/junk/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package HUnit -package test-framework -package test-framework-hunit -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/junk/ExtensionTests.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/junk/LexicalSyntaxTests.o : hssqlppp/tests/Database/HsSqlPpp/Tests/junk/LexicalSyntaxTests.lhs
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/junk/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/junk/LexicalSyntaxTests.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/junk/PrecisionTests.o : hssqlppp/tests/Database/HsSqlPpp/Tests/junk/PrecisionTests.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/junk/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package HUnit -package base -package test-framework -package test-framework-hunit -package uniplate -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/junk/PrecisionTests.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Tests/junk/RoundtripTests.o : hssqlppp/tests/Database/HsSqlPpp/Tests/junk/RoundtripTests.lhs \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/Here.hi \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi \
    $(BUILD)/Database/HsSqlPpp/Ast.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Tests/junk/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package HUnit -package base -package mtl -package syb -package test-framework -package test-framework-hunit -package uniplate -c $< -o $(BUILD)/Database/HsSqlPpp/Tests/junk/RoundtripTests.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.o : hssqlppp/tests/Database/HsSqlPpp/Utils/GroomUtils.lhs
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Utils/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package haskell-src-exts -package uniplate -c $< -o $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Utils/Here.o : hssqlppp/tests/Database/HsSqlPpp/Utils/Here.lhs
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Utils/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package template-haskell -c $< -o $(BUILD)/Database/HsSqlPpp/Utils/Here.o \
        -i$(BUILD)/

$(BUILD)/Tests.o : hssqlppp/tests/Tests.lhs \
    $(BUILD)/Database/HsSqlPpp/Tests/Tests.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package test-framework -c $< -o $(BUILD)/Tests.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Utils/CatalogReader.o : hssqlppp-pg/src/Database/HsSqlPpp/Utils/CatalogReader.lhs \
    $(BUILD)/Database/HsSqlPpp/Utils/Here2.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/PgUtils.hi
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Utils/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package split -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Utils/CatalogReader.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Utils/Here2.o : hssqlppp-pg/src/Database/HsSqlPpp/Utils/Here2.lhs
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Utils/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package template-haskell -c $< -o $(BUILD)/Database/HsSqlPpp/Utils/Here2.o \
        -i$(BUILD)/

$(BUILD)/Database/HsSqlPpp/Utils/PgUtils.o : hssqlppp-pg/src/Database/HsSqlPpp/Utils/PgUtils.lhs
	-mkdir -p $(BUILD)/Database/HsSqlPpp/Utils/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package HDBC -package HDBC-postgresql -package base -package text -c $< -o $(BUILD)/Database/HsSqlPpp/Utils/PgUtils.o \
        -i$(BUILD)/

$(BUILD)/MakeDefaultTemplate1Catalog.o : build-src/MakeDefaultTemplate1Catalog.lhs \
    $(BUILD)/Database/HsSqlPpp/Utils/Here.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/CatalogReader.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/PgUtils.hi \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package groom -package text -c $< -o $(BUILD)/MakeDefaultTemplate1Catalog.o \
        -i$(BUILD)/

$(BUILD)/PostprocessUuagc.o : build-src/PostprocessUuagc.lhs \
    $(BUILD)/UUAGCHaddocks.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/PostprocessUuagc.o \
        -i$(BUILD)/

$(BUILD)/UUAGCHaddocks.o : build-src/UUAGCHaddocks.lhs
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package directory -c $< -o $(BUILD)/UUAGCHaddocks.o \
        -i$(BUILD)/

$(BUILD)/Experimentation.o : examples/Experimentation.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package groom -package text -c $< -o $(BUILD)/Experimentation.o \
        -i$(BUILD)/

$(BUILD)/FixSqlServerTpchSyntax.o : examples/FixSqlServerTpchSyntax.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Pretty.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package uniplate -c $< -o $(BUILD)/FixSqlServerTpchSyntax.o \
        -i$(BUILD)/

$(BUILD)/Lex.o : examples/Lex.lhs \
    $(BUILD)/Database/HsSqlPpp/Utility.hi \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package groom -package text -c $< -o $(BUILD)/Lex.o \
        -i$(BUILD)/

$(BUILD)/MakeSelect.o : examples/MakeSelect.lhs \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Pretty.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/MakeSelect.o \
        -i$(BUILD)/

$(BUILD)/PPPTest.o : examples/PPPTest.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/Pretty.hi \
    $(BUILD)/Database/HsSqlPpp/Utility.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package Diff -package base -package groom -package text -package uniplate -c $< -o $(BUILD)/PPPTest.o \
        -i$(BUILD)/

$(BUILD)/Parse.o : examples/Parse.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/Parse.o \
        -i$(BUILD)/

$(BUILD)/Parse2.o : examples/Parse2.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package groom -package text -c $< -o $(BUILD)/Parse2.o \
        -i$(BUILD)/

$(BUILD)/Parse3.o : examples/Parse3.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package groom -package text -c $< -o $(BUILD)/Parse3.o \
        -i$(BUILD)/

$(BUILD)/QQ.o : examples/QQ.lhs \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi \
    $(BUILD)/Database/HsSqlPpp/Pretty.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -c $< -o $(BUILD)/QQ.o \
        -i$(BUILD)/

$(BUILD)/ShowCatalog.o : examples/ShowCatalog.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package groom -c $< -o $(BUILD)/ShowCatalog.o \
        -i$(BUILD)/

$(BUILD)/TypeCheck.o : examples/TypeCheck.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi \
    $(BUILD)/Database/HsSqlPpp/Types.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi \
    $(BUILD)/Database/HsSqlPpp/Ast.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/TypeCheck.o \
        -i$(BUILD)/

$(BUILD)/TypeCheck2.o : examples/TypeCheck2.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi \
    $(BUILD)/Database/HsSqlPpp/Types.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Utility.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/TypeCheck2.o \
        -i$(BUILD)/

$(BUILD)/TypeCheck3.o : examples/TypeCheck3.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.hi \
    $(BUILD)/Database/HsSqlPpp/Tests/TpchData.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/TypeCheck3.o \
        -i$(BUILD)/

$(BUILD)/TypeCheckDB.o : examples/TypeCheckDB.lhs \
    $(BUILD)/Database/HsSqlPpp/Parser.hi \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.hi \
    $(BUILD)/Database/HsSqlPpp/Catalog.hi \
    $(BUILD)/Database/HsSqlPpp/Types.hi \
    $(BUILD)/Database/HsSqlPpp/Annotation.hi \
    $(BUILD)/Database/HsSqlPpp/Ast.hi \
    $(BUILD)/Database/HsSqlPpp/Utils/CatalogReader.hi
	-mkdir -p $(BUILD)/
	$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ -package base -package text -c $< -o $(BUILD)/TypeCheckDB.o \
        -i$(BUILD)/
$(BUILD)/Tests : $(BUILD)/Tests.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/Pretty.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/CreateTable.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Dml.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Joins.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Misc.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/MiscDdl.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/ParserTests.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Plpgsql.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/SelectLists.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/SqlServer.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/TableRefs.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.o \
    $(BUILD)/Database/HsSqlPpp/Tests/QuasiQuoteTests.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Tests.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TpchData.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Issues.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Joins.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TSQL.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Tpch.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.o \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.o \
    $(BUILD)/Database/HsSqlPpp/Types.o \
    $(BUILD)/Database/HsSqlPpp/Utility.o \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Here.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(TESTS_EXTRA) \
    -o $(BUILD)/Tests \
    $(BUILD)/Tests.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/Pretty.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/CombineQueryExprs.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/CreateTable.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Dml.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/FunctionsDdl.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Joins.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Misc.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/MiscDdl.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/MiscQueryExprs.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/ParserTests.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Plpgsql.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/ScalarExprs.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/SelectLists.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/SqlServer.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/TableRefs.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Parsing/Utils.o \
    $(BUILD)/Database/HsSqlPpp/Tests/QuasiQuoteTests.o \
    $(BUILD)/Database/HsSqlPpp/Tests/Tests.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TpchData.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/CaseExpressions.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/ImplicitCasts.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Issues.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Joins.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Rewrites.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/ScalarExprs.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/SimpleQueryExprs.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TSQL.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Tpch.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TrefIdentifiers.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/TypeCheckTests.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TypeChecking/Utils.o \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.o \
    $(BUILD)/Database/HsSqlPpp/Types.o \
    $(BUILD)/Database/HsSqlPpp/Utility.o \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Here.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
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

$(BUILD)/MakeDefaultTemplate1Catalog : $(BUILD)/MakeDefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Utils/CatalogReader.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Here.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Here2.o \
    $(BUILD)/Database/HsSqlPpp/Utils/PgUtils.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(MAKEDEFAULTTEMPLATE1CATALOG_EXTRA) \
    -o $(BUILD)/MakeDefaultTemplate1Catalog \
    $(BUILD)/MakeDefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Utils/CatalogReader.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Here.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Here2.o \
    $(BUILD)/Database/HsSqlPpp/Utils/PgUtils.o \
    -hide-all-packages \
    -package HDBC \
    -package HDBC-postgresql \
    -package base \
    -package containers \
    -package groom \
    -package split \
    -package template-haskell \
    -package text \
    -package uniplate

$(BUILD)/PostprocessUuagc : $(BUILD)/PostprocessUuagc.o \
    $(BUILD)/UUAGCHaddocks.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(POSTPROCESSUUAGC_EXTRA) \
    -o $(BUILD)/PostprocessUuagc \
    $(BUILD)/PostprocessUuagc.o \
    $(BUILD)/UUAGCHaddocks.o \
    -hide-all-packages \
    -package base \
    -package directory

$(BUILD)/MakeSelect : $(BUILD)/MakeSelect.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Pretty.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(MAKESELECT_EXTRA) \
    -o $(BUILD)/MakeSelect \
    $(BUILD)/MakeSelect.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Pretty.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package containers \
    -package groom \
    -package mtl \
    -package pretty \
    -package syb \
    -package text \
    -package uniplate

$(BUILD)/Parse : $(BUILD)/Parse.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(PARSE_EXTRA) \
    -o $(BUILD)/Parse \
    $(BUILD)/Parse.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package containers \
    -package groom \
    -package mtl \
    -package parsec \
    -package syb \
    -package text \
    -package uniplate

$(BUILD)/Parse2 : $(BUILD)/Parse2.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(PARSE2_EXTRA) \
    -o $(BUILD)/Parse2 \
    $(BUILD)/Parse2.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package containers \
    -package groom \
    -package haskell-src-exts \
    -package mtl \
    -package parsec \
    -package syb \
    -package text \
    -package uniplate

$(BUILD)/Parse3 : $(BUILD)/Parse3.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(PARSE3_EXTRA) \
    -o $(BUILD)/Parse3 \
    $(BUILD)/Parse3.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package containers \
    -package groom \
    -package haskell-src-exts \
    -package mtl \
    -package parsec \
    -package syb \
    -package text \
    -package uniplate

$(BUILD)/Lex : $(BUILD)/Lex.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Utility.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(LEX_EXTRA) \
    -o $(BUILD)/Lex \
    $(BUILD)/Lex.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Utility.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package containers \
    -package groom \
    -package mtl \
    -package parsec \
    -package syb \
    -package text \
    -package uniplate

$(BUILD)/TypeCheck3 : $(BUILD)/TypeCheck3.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TpchData.o \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.o \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Here.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(TYPECHECK3_EXTRA) \
    -o $(BUILD)/TypeCheck3 \
    $(BUILD)/TypeCheck3.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Tests/TpchData.o \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.o \
    $(BUILD)/Database/HsSqlPpp/Utils/GroomUtils.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Here.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
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

$(BUILD)/TypeCheck2 : $(BUILD)/TypeCheck2.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.o \
    $(BUILD)/Database/HsSqlPpp/Types.o \
    $(BUILD)/Database/HsSqlPpp/Utility.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(TYPECHECK2_EXTRA) \
    -o $(BUILD)/TypeCheck2 \
    $(BUILD)/TypeCheck2.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.o \
    $(BUILD)/Database/HsSqlPpp/Types.o \
    $(BUILD)/Database/HsSqlPpp/Utility.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package containers \
    -package groom \
    -package mtl \
    -package parsec \
    -package syb \
    -package text \
    -package uniplate

$(BUILD)/TypeCheck : $(BUILD)/TypeCheck.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.o \
    $(BUILD)/Database/HsSqlPpp/Types.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(TYPECHECK_EXTRA) \
    -o $(BUILD)/TypeCheck \
    $(BUILD)/TypeCheck.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.o \
    $(BUILD)/Database/HsSqlPpp/Types.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
    -package base \
    -package containers \
    -package groom \
    -package mtl \
    -package parsec \
    -package syb \
    -package text \
    -package uniplate

$(BUILD)/TypeCheckDB : $(BUILD)/TypeCheckDB.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.o \
    $(BUILD)/Database/HsSqlPpp/Types.o \
    $(BUILD)/Database/HsSqlPpp/Utils/CatalogReader.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Here2.o \
    $(BUILD)/Database/HsSqlPpp/Utils/PgUtils.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(TYPECHECKDB_EXTRA) \
    -o $(BUILD)/TypeCheckDB \
    $(BUILD)/TypeCheckDB.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/TypeChecker.o \
    $(BUILD)/Database/HsSqlPpp/Types.o \
    $(BUILD)/Database/HsSqlPpp/Utils/CatalogReader.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Here2.o \
    $(BUILD)/Database/HsSqlPpp/Utils/PgUtils.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
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

$(BUILD)/PPPTest : $(BUILD)/PPPTest.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/Pretty.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Utility.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o
	-mkdir -p $(BUILD)/
	$(HL) $(HL_OPTS) $(PPPTEST_EXTRA) \
    -o $(BUILD)/PPPTest \
    $(BUILD)/PPPTest.o \
    $(BUILD)/Database/HsSqlPpp/Annotation.o \
    $(BUILD)/Database/HsSqlPpp/Ast.o \
    $(BUILD)/Database/HsSqlPpp/Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/AstInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/CatalogInternal.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTSQLCatalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/Catalog/DefaultTemplate1Catalog.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/Environment.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTediousTypeUtils.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/OldTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/SqlTypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypeChecking/TypeConversion.o \
    $(BUILD)/Database/HsSqlPpp/Internals/TypesInternal.o \
    $(BUILD)/Database/HsSqlPpp/Parser.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/Lexer.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParseErrors.o \
    $(BUILD)/Database/HsSqlPpp/Parsing/ParserInternal.o \
    $(BUILD)/Database/HsSqlPpp/Pretty.o \
    $(BUILD)/Database/HsSqlPpp/SqlDialect.o \
    $(BUILD)/Database/HsSqlPpp/Utility.o \
    $(BUILD)/Database/HsSqlPpp/Utils/Utils.o \
    -hide-all-packages \
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


%.hi : %.o
	@:
