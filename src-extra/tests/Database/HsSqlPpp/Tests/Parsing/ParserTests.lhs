
The automated tests, uses hunit to check a bunch of text expressions
and sql statements parse to the correct tree, and then checks pretty
printing and then reparsing gives the same tree. The code was mostly
written almost in tdd style, which the order/ coverage of these tests
reflects.

There are no tests for invalid syntax at the moment.

> module Database.HsSqlPpp.Tests.Parsing.ParserTests
>     (parserTests
>     ,parserTestData
>     ,Item(..)
>     ) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Generics
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Pretty
>

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.Parsing.Expressions
> import Database.HsSqlPpp.Tests.Parsing.Selects
> import Database.HsSqlPpp.Tests.Parsing.Dml
> import Database.HsSqlPpp.Tests.Parsing.CreateTable
> import Database.HsSqlPpp.Tests.Parsing.MiscDdl
> import Database.HsSqlPpp.Tests.Parsing.FunctionsDdl
> import Database.HsSqlPpp.Tests.Parsing.Plpgsql
> import Database.HsSqlPpp.Tests.Parsing.Misc

> parserTests :: Test.Framework.Test
> parserTests = itemToTft parserTestData
>
> parserTestData :: Item
> parserTestData =
>   Group "parserTests" [
>              expressionParsingTestData
>             ,selectParsingTestData
>             ,dmlParsingTestData
>             ,Group "ddl" [createTableParsingTestData
>                          ,miscDdlParsingTestData
>                          ,functionsDdlParsingTestData]
>             ,pgplsqlParsingTestData
>             ,miscParserTestData
>             ]

--------------------------------------------------------------------------------

Unit test helpers

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Expr a b) = testParseScalarExpr a b
> itemToTft (PgSqlStmt a b) = testParsePlpgsqlStatements a b
> itemToTft (Stmt a b) = testParseStatements a b
> itemToTft (MSStmt a b) = testParseMSStatements a b
> itemToTft (Group s is) = testGroup s $ map itemToTft is
>
> testParseScalarExpr :: String -> ScalarExpr -> Test.Framework.Test
> testParseScalarExpr src ast =
>   parseUtil src ast (parseScalarExpr "") (parseScalarExpr "") printScalarExpr
>
> testParseStatements :: String -> [Statement] -> Test.Framework.Test
> testParseStatements src ast =
>   parseUtil src ast (parseStatements "") (parseStatements "") printStatements
>
> testParseMSStatements :: String -> [Statement] -> Test.Framework.Test
> testParseMSStatements src ast =
>   parseUtil src ast parseMsQuery (parseStatements "") printStatements
>   where
>     parseMsQuery :: String -> Either ParseErrorExtra [Statement]
>     parseMsQuery s =
>       (\p' -> [QueryStatement ea p'])
>       `fmap` parseSqlServerQueryExpr "" s

>
> testParsePlpgsqlStatements :: String -> [Statement] -> Test.Framework.Test
> testParsePlpgsqlStatements src ast =
>   parseUtil src ast (parsePlpgsql "") (parsePlpgsql "") printStatements
>
> parseUtil :: (Show t, Eq b, Show b, Data b) =>
>              String
>           -> b
>           -> (String -> Either t b)
>           -> (String -> Either t b)
>           -> (b -> String)
>           -> Test.Framework.Test
> parseUtil src ast parser reparser printer = testCase ("parse " ++ src) $
>   case parser src of
>     Left er -> assertFailure $ show er
>     Right ast' -> do
>       assertEqual ("parse " ++ src) ast $ resetAnnotations ast'
>       case reparser (printer ast) of
>         Left er -> assertFailure $ "reparse\n" ++ show er ++ "\n" -- ++ pp ++ "\n"
>         Right ast'' -> assertEqual ("reparse " ++ printer ast) ast $ resetAnnotations ast''

~~~~
TODO
new idea for testing:
parsesql -> ast1
parse, pretty print, parse -> ast2
load into pg, pg_dump, parse -> ast3
parse, pretty print, load into pg, pg_dump, parse -> ast4
check all these asts are the same
~~~~
