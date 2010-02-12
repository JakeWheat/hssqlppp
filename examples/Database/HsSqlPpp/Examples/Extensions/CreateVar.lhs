Copyright 2010 Jake Wheat

Extension to remove the boilerplate from adding tables with a single
attribute and single row, a bit like a global variable in the
database.

> {-# LANGUAGE ViewPatterns, QuasiQuotes #-}
>
> module Database.HsSqlPpp.Examples.Extensions.CreateVar
>     where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.SqlQuote
> import Database.HsSqlPpp.PrettyPrinter
> import Database.HsSqlPpp.Examples.Extensions.CreateAssertion
>
> createVarExample :: ExtensionTest
> createVarExample = ExtensionTest
>   "CreateVar"
>   (createAssertion . createVar)
>   [$sqlStmts| select create_var('varname', 'vartype'); |]
>   (createAssertion [$sqlStmts|
>
>   create table varname_table (
>     varname vartype primary key
>   );
>
>   create function get_varname() returns vartype as $a$
>     select * from varname_table;
>   $a$ language sql stable;
>
>   select create_assertion('varname_table_01_tuple',
>                           '(select count(*) from varname_table) <= 1');
>
>   |])
>
> createVar :: Data a => a -> a
> createVar =
>     transformBi $ \x ->
>       case x of
>         [$sqlStmt| select "create_var"($s(varname)
>                                       ,$s(typename)); |] : tl
>             -> let tablename = varname ++ "_table"
>                    fnname = "get_" ++ varname
>                    conname = varname ++ "_table_01_tuple"
>                    expr = printExpression
>                              [$sqlExpr| (select count(*) from $(tablename)) <= 1 |]
>                in [$sqlStmts|
>
>   create table $(tablename) (
>    $(varname) $(typename) primary key
>   );
>
>   create function $(fnname)() returns $(typename) as $a$
>     select * from $(tablename);
>   $a$ language sql stable;
>
>   select create_assertion($s(conname), $s(expr));
>
>                    |] ++ tl
>         x1 -> x1
>
