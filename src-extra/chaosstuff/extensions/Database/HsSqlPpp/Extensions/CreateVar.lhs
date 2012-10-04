
Extension to remove the boilerplate from adding tables with a single
attribute and single row, a bit like a global variable in the
database.

> {-# LANGUAGE QuasiQuotes #-}
>
> module Database.HsSqlPpp.Extensions.CreateVar
>     where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.Quote
> import Database.HsSqlPpp.Pretty
> import Database.HsSqlPpp.Extensions.CreateAssertion
> import Database.HsSqlPpp.Extensions.AstUtils
>
> createVarExample :: ExtensionTest
> createVarExample = ExtensionTest
>   "CreateVar"
>   (createAssertion . createVar)
>   [sqlStmts| select create_var('varname', 'vartype'); |]
>   (createAssertion [sqlStmts|
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
>         s@[sqlStmt| select create_var($e(varnamex)
>                                      ,$e(typenamex)); |] : tl
>             -> let StringLit _ varname = varnamex
>                    varnamey = Nmc varname
>                    StringLit _ typename = typenamex
>                    typenamey = Name emptyAnnotation [Nmc typename]
>                    tablename = Name emptyAnnotation [Nmc $ varname ++ "_table"]
>                    fnname = Name emptyAnnotation [Nmc $ "get_" ++ varname]
>                    conname = StringLit emptyAnnotation $  varname ++ "_table_01_tuple"
>                    expr = StringLit emptyAnnotation $ printScalarExpr defaultPPFlags
>                              [sqlExpr| (select count(*) from $n(tablename)) <= 1 |]
>                in replaceSourcePos s [sqlStmts|
>
>   create table $n(tablename) (
>    $m(varnamey) $n(typenamey) primary key
>   );
>
>   create function $n(fnname)() returns $n(typenamey) as $a$
>     select * from $n(tablename);
>   $a$ language sql stable;
>
>   select create_assertion($e(conname), $e(expr));
>
>                    |] ++ tl
>         x1 -> x1
>
