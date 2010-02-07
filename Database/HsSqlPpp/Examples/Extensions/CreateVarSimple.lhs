Copyright 2010 Jake Wheat

Demonstration extension: createVarSimple
========================================

Takes a name and a type and creates a table with a single attribute
with that name and type.

See [ExtensionsUtils](ExtensionsUtils.lhs.html) for the support
functions.

> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.Examples.Extensions.CreateVarSimple
>     where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.SqlQuote

Example/ Test
-------------

Here is the example code which demonstrates what the extension is
supposed to do; it doubles as an automated test.

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
>     [$sqlQuote| select create_var('varname', 'vartype'); |]
>
>     -- what the example SQL should be transformed into:
>     [$sqlQuote|
>
>  create table varname_table (
>      varname vartype
>  );
>
>      |]

Ast transform function
----------------------

> createVarSimple :: Data a => a -> a
> createVarSimple =

We look for a function call with the function name "create_var" and
two string literal arguments (calls to create_var which don't use
exactly two string literals will be silently ignored by this code).

We want to replace it with a create table statement.

A view pattern is used which removes a load of clutter compared with
using vanilla pattern matching.

>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView _
>                                     "create_var"
>                                     [StringLit _ _ varname
>                                     ,StringLit _ _ typename]):tl
>             -> let tablename = varname ++ "_table"
>                in [$sqlQuote|
>
>   create table $(tablename) (
>    $(varname) $(typename)
>   );
>
>                    |] ++ tl
>         x1 -> x1
>
