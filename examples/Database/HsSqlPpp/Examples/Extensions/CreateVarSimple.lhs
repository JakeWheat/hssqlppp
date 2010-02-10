Copyright 2010 Jake Wheat

Demonstration extension: createVarSimple
========================================

Takes a name and a type and creates a table with a single attribute
with that name and type.

> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.Examples.Extensions.CreateVarSimple
>     where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.SqlQuote

Example/ Test
-------------

> createVarSimpleExample :: ExtensionTest
> createVarSimpleExample =
>   ExtensionTest
>
>     -- name of the extension, used when running the tests
>     "CreateVarSimple"
>
>     -- the transformation function itself, implemented below
>     createVarSimple
>
>     -- example of the SQL we want to replace
>     [$sqlStmts| select create_var('varname', 'vartype'); |]
>
>     -- what the example SQL should be transformed into:
>     [$sqlStmts|
>
>       create table varname_table (
>         varname vartype
>       );
>
>      |]

Ast transform function
----------------------

We look for a function call with the function name "create_var" and
two string literal arguments (calls to create_var which don't use
exactly two string literals will be silently ignored by this code).

We want to replace it with a create table statement.

> createVarSimple :: Data a => a -> a
> createVarSimple =
>     transformBi $ \x ->
>       case x of
>         [$sqlStmt| select create_var($s(varname), $s(typename)); |]
>             -> let tablename = varname ++ "_table"
>                in [$sqlStmt|
>
>   create table $(tablename) (
>    $(varname) $(typename)
>   );
>
>                    |]
>         x1 -> x1
>
