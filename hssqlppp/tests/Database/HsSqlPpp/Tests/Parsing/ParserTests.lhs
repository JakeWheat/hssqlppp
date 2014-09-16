
The automated tests, uses hunit to check a bunch of text expressions
and sql statements parse to the correct tree, and then checks pretty
printing and then reparsing gives the same tree. The code was mostly
written almost in tdd style, which the order/ coverage of these tests
reflects.

There are no tests for invalid syntax at the moment.

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.ParserTests
>     (parserTestData
>     ,Item(..)
>     ) where
>
> --import Data.Generics
> --import Control.Monad
> --import Database.HsSqlPpp.Ast
> --import Database.HsSqlPpp.Annotation
> --import Database.HsSqlPpp.Parser
> --import Database.HsSqlPpp.Pretty
> --import Database.HsSqlPpp.Utility
>

> --import Database.HsSqlPpp.Tests.Parsing.Utils
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
> import Database.HsSqlPpp.Tests.Parsing.Oracle
> import Database.HsSqlPpp.Tests.Parsing.LexerTests

> --import Control.Applicative
> import Database.HsSqlPpp.Tests.TestTypes


> --import Database.HsSqlPpp.Tests.TestUtils
> --import Data.Text.Lazy (Text)
> --import qualified Data.Text as T
> --import qualified Data.Text.Lazy as L

> parserTestData :: Item
> parserTestData =
>   Group "parserTests" [
>              lexerTests
>             ,scalarExprs
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
>             ,oracle
>             ]

