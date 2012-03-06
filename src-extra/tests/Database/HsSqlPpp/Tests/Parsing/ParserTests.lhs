
The automated tests, uses hunit to check a bunch of text expressions
and sql statements parse to the correct tree, and then checks pretty
printing and then reparsing gives the same tree. The code was mostly
written almost in tdd style, which the order/ coverage of these tests
reflects.

There are no tests for invalid syntax at the moment.

> {-# LANGUAGE OverloadedStrings #-}
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
> import Control.Monad
> import Database.HsSqlPpp.Ast
> --import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Pretty
> import Database.HsSqlPpp.Utility
>
> import Database.HsSqlPpp.Utils.GroomUtils

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.Parsing.ScalarExprs
> import Database.HsSqlPpp.Tests.Parsing.MiscQueryExprs
> import Database.HsSqlPpp.Tests.Parsing.CombineQueryExprs
> import Database.HsSqlPpp.Tests.Parsing.SelectLists
> import Database.HsSqlPpp.Tests.Parsing.TableRefs
> import Database.HsSqlPpp.Tests.Parsing.Joins

> import Database.HsSqlPpp.Tests.Parsing.Dml
> import Database.HsSqlPpp.Tests.Parsing.Misc

> import Database.HsSqlPpp.Tests.Parsing.CreateTable
> import Database.HsSqlPpp.Tests.Parsing.MiscDdl
> import Database.HsSqlPpp.Tests.Parsing.FunctionsDdl
> import Database.HsSqlPpp.Tests.Parsing.Plpgsql

> import Database.HsSqlPpp.Tests.Parsing.SqlServer

> import Database.HsSqlPpp.Tests.TestUtils
> import Data.Text (Text)
> import qualified Data.Text as T

> parserTests :: Test.Framework.Test
> parserTests = itemToTft parserTestData
>
> parserTestData :: Item
> parserTestData =
>   Group "parserTests" [
>              scalarExprs
>             ,miscQueryExprs
>             ,combineQueryExprs
>             ,selectLists
>             ,tableRefs
>             ,joins
>             ,dml
>             ,Group "ddl" [createTable
>                          ,miscDdl
>                          ,functionsDdl]
>             ,pgplsql
>             ,misc
>             ,sqlServer
>             ]

--------------------------------------------------------------------------------

Unit test helpers

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Expr a b) = testParseScalarExpr a b
> itemToTft (QueryExpr a b) = testParseQueryExpr a b
> itemToTft (PgSqlStmt a b) = testParsePlpgsqlStatements PostgreSQLDialect a b
> itemToTft (Stmt a b) = testParseStatements PostgreSQLDialect a b
> itemToTft (TSQL a b) =
>   testParsePlpgsqlStatements (if True
>                        then SQLServerDialect
>                        else PostgreSQLDialect) a b
> --itemToTft (MSStmt a b) = testParseStatements a b
> itemToTft (Group s is) = testGroup (T.unpack s) $ map itemToTft is
>
> testParseScalarExpr :: Text -> ScalarExpr -> Test.Framework.Test
> testParseScalarExpr src ast =
>   parseUtil src ast (parseScalarExpr defaultParseFlags "" Nothing)
>                     (parseScalarExpr defaultParseFlags "" Nothing)
>                     (printScalarExpr defaultPPFlags)
> testParseQueryExpr :: Text -> QueryExpr -> Test.Framework.Test
> testParseQueryExpr src ast =
>   parseUtil src ast (parseQueryExpr defaultParseFlags "" Nothing)
>                     (parseQueryExpr defaultParseFlags "" Nothing)
>                     (printQueryExpr defaultPPFlags)

>
> testParseStatements :: SQLSyntaxDialect -> Text -> [Statement] -> Test.Framework.Test
> testParseStatements flg src ast =
>   let parse = parseStatements defaultParseFlags {pfDialect=flg} "" Nothing
>       pp = printStatements defaultPPFlags {ppDialect=flg}
>   in parseUtil src ast parse parse pp
>
> testParsePlpgsqlStatements :: SQLSyntaxDialect -> Text -> [Statement] -> Test.Framework.Test
> testParsePlpgsqlStatements flg src ast =
>   parseUtil src ast (parsePlpgsql defaultParseFlags {pfDialect=flg} "" Nothing)
>                     (parsePlpgsql defaultParseFlags {pfDialect=flg} "" Nothing)
>                     (printStatements defaultPPFlags {ppDialect=flg})
>
> parseUtil :: (Show t, Eq b, Show b, Data b) =>
>              Text
>           -> b
>           -> (Text -> Either t b)
>           -> (Text -> Either t b)
>           -> (b -> Text)
>           -> Test.Framework.Test
> parseUtil src ast parser reparser printer = testCase ("parse " ++ T.unpack src) $
>   case parser src of
>     Left er -> assertFailure $ show er
>     Right ast' -> do
>       when (ast /= resetAnnotations ast') $ do
>         putStrLn $ groomNoAnns ast
>         putStrLn $ groomNoAnns $ resetAnnotations ast'
>       assertEqual ("parse " ++ T.unpack src) ast $ resetAnnotations ast'
>       case reparser (printer ast) of
>         Left er -> assertFailure $ "reparse\n" ++ (T.unpack $ printer ast) ++ "\n" ++ show er ++ "\n" -- ++ pp ++ "\n"
>         Right ast'' -> assertEqual ("reparse: " ++ T.unpack (printer ast)) ast $ resetAnnotations ast''

~~~~
TODO
new idea for testing:
parsesql -> ast1
parse, pretty print, parse -> ast2
load into pg, pg_dump, parse -> ast3
parse, pretty print, load into pg, pg_dump, parse -> ast4
check all these asts are the same
~~~~
