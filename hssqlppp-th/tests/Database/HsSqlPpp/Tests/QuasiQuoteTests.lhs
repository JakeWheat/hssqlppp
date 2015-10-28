
Tests mainly for antiquotation, plus examples of where antiquotes work.

> {-# LANGUAGE QuasiQuotes,ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.Tests.QuasiQuoteTests (quasiQuoteTests, quasiQuoteTestData, Item(..)) where
>
> import qualified Test.Tasty as T
> import qualified Test.Tasty.HUnit as H

> import Data.Data
>
> import Database.HsSqlPpp.Syntax
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Pretty
> import Database.HsSqlPpp.Quote
> import Database.HsSqlPpp.Utility
> --import Database.HsSqlPpp.Tests.TestUtils
> import qualified Data.Text.Lazy as L
>
> data Item = Expr ScalarExpr ScalarExpr
>           | Stmts [Statement] [Statement]
>           | PgSqlStmts [Statement] [Statement]
>           | Stmt Statement Statement
>           | PgSqlStmt Statement Statement
>           | Group String [Item]

> quasiQuoteTests :: T.TestTree
> quasiQuoteTests = itemToTft quasiQuoteTestData
>
> quasiQuoteTestData :: Item
> quasiQuoteTestData =
>   Group "quasiQuoteTests" [

>     let tableName = [sqlName| my_table |]
>         varname = [sqlNameComponent| my_field |]
>         typename = [sqlName| text |]
>     in Stmt [sqlStmt|
>
>      create table $n(tableName) (
>        $m(varname) $n(typename)
>      );
>
>      |]
>      [sqlStmt|
>      create table my_table (
>        my_field text
>      );
>      |]


>     ,let fnname = [sqlName| my_function |]
>          tablename = [sqlName| my_table |]
>          typename = [sqlName| int |]
>      in Stmt [sqlStmt|
>
>   create function $n(fnname)() returns $n(typename) as $a$
>     select * from $n(tablename);
>   $a$ language sql stable;
>
>      |]
>      [sqlStmt|
>   create function my_function() returns int as $a$
>     select * from my_table;
>   $a$ language sql stable;
>      |]


>     ,let fnname = [sqlName|my_function|]
>      in Stmt [sqlStmt| drop function $n(fnname)();|]
>              [sqlStmt| drop function my_function();|]
>
>     ,let expr = StringLit ea "testing"
>      in PgSqlStmt [pgsqlStmt| return $e(expr); |]
>                   [pgsqlStmt| return 'testing'; |]

>     ,let expr = [sqlExpr| 3 + 4 |]
>      in PgSqlStmt [pgsqlStmt| return $e(expr); |]
>                   [pgsqlStmt| return 3 + 4; |]
>

>     ,let triggername = [sqlNameComponent|my_trigger|]
>          tablename = [sqlName|my_table|]
>          opname = [sqlName|my_function|]
>      in Stmt [sqlStmt|
>   create trigger $m(triggername)
>     after insert or update or delete on $n(tablename)
>     for each statement
>     execute procedure $n(opname)();
>             |]
>              [sqlStmt|
>   create trigger my_trigger
>     after insert or update or delete on my_table
>     for each statement
>     execute procedure my_function();
>             |]

>     ,let tablename = [sqlName|lotsastuff|]
>      in Expr [sqlExpr|(select count(*) from $n(tablename))|]
>              [sqlExpr|(select count(*) from lotsastuff)|]
>
>     ,let trigname = [sqlNameComponent|tbl_trig1|]
>          tablename = [sqlName|tbl|]
>          tevent = TUpdate
>          fn = [sqlName|checkit|]
>      in Stmt [sqlStmt|
>      create trigger $m(trigname)
>         after $t(tevent) on $n(tablename)
>         for each row
>         execute procedure $n(fn)();
>             |] [sqlStmt|
>      create trigger tbl_trig1
>         after update on tbl
>         for each row
>         execute procedure checkit();
>             |]
>     ,let x = [sqlName| fnname |]
>      in Expr [sqlExpr| $n(x)('a') |]
>              [sqlExpr| fnname('a') |]
>     ,let x = StringLit ea "splicedstring"
>      in Expr [sqlExpr| $e(x) |]
>              [sqlExpr| 'splicedstring' |]
>     ,let x = [sqlName|splicedIdentifier|]
>      in Expr [sqlExpr| $n(x) |]
>              [sqlExpr| splicedIdentifier |]
>     ,let errMsg = "string splice"
>      in PgSqlStmt [pgsqlStmt| raise exception $s(errMsg); |]
>                   [pgsqlStmt| raise exception 'string splice'; |]

>

--------------------------------------------------------------------------------

expressions

>   ]


================================================================================

Unit test helpers

> itemToTft :: Item -> T.TestTree
> itemToTft (Expr a b) = H.testCase (L.unpack $ prettyScalarExpr defaultPrettyFlags b) $ stripEqual a b
> itemToTft (PgSqlStmt a b) = H.testCase (L.unpack $ prettyStatements defaultPrettyFlags [b]) $ stripEqual a b
> itemToTft (Stmt a b) = H.testCase (L.unpack $ prettyStatements defaultPrettyFlags [b]) $  stripEqual a b
> itemToTft (PgSqlStmts a b) = H.testCase (L.unpack $ prettyStatements defaultPrettyFlags b) $ stripEqual a b
> itemToTft (Stmts a b) = H.testCase (L.unpack $ prettyStatements defaultPrettyFlags b) $ stripEqual a b
> itemToTft (Group s is) = T.testGroup s $ map itemToTft is
> stripEqual :: (Data a, Eq a, Show a) =>
>               a -> a -> H.Assertion
> stripEqual a b = H.assertEqual "" (resetAnnotations a) (resetAnnotations b)

> ea :: Annotation
> ea = emptyAnnotation
