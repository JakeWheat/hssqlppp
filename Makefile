
# todo: get all the sh files and builds and scripts and put them all
# in here instead


#todo: find something better than make

src/Database/HsSqlPpp/Internals/AstAnti.hs : src/Database/HsSqlPpp/Internals/AstInternal.hs
	ghc -isrc-extra/util:src-extra/devel-util src-extra/devel-util/MakeAntiNodesRunner.lhs
	src-extra/devel-util/MakeAntiNodesRunner

src/Database/HsSqlPpp/Internals/AstInternal.hs : src/Database/HsSqlPpp/Internals/AstInternal.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/FixUpIdentifiers.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/QueryStatement.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/TableRefs.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/QueryExprs/SelectLists.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/Misc.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/ScalarExprs/ScalarExprs.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/Plpgsql/Plpgsql.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/Plpgsql/Block.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/Dml/Update.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/Dml/Insert.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/Dml/Delete.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/Statements.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/Ddl/Drops.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/Ddl/CreateFunction.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/Ddl/Triggers.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/Ddl/MiscCreates.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/Ddl/CreateTable.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/ParameterizedStatements.ag \
		src/Database/HsSqlPpp/Internals/TypeChecking/TypeChecking.ag
	uuagc  -dcfwsp -P src/Database/HsSqlPpp/Internals/ src/Database/HsSqlPpp/Internals/AstInternal.ag


