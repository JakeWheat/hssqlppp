Copyright 2010 Jake Wheat

Demonstration extension: createVar
==================================

Extension to remove the boilerplate from adding tables with a single
attribute and single row, a bit like a global variable in the
database.

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
> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.Examples.Extensions.SQLCode

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
>     "select create_var('varname', 'vartype');"
>
>     -- what the example SQL should be transformed into:
>     [$here|
>
>  create table varname_table (
>      varname vartype
>  );
>
>  create function get_varname() returns vartype as $a$
>      select * from varname_table;
>  $a$ language sql stable;
>
>      |]

Ast transform function
----------------------

> createVarSimple :: Data a => a -> a
> createVarSimple =

We look for a function call with the function name "create_var" and
two string literal arguments (calls to create_var which don't use
exactly two string literals will be silently ignored by this code).

A view pattern is used which removes a load of clutter compared with
using vanilla pattern matching.

>     transformBi $ \x ->
>       case x of
>         (funCallView -> FunCallView _
>                                     "create_var"
>                                     [StringLit _ _ varname
>                                     ,StringLit _ _ typename]):tl
>             -> let tablename = varname ++ "_table"
>                    fnname = "get_" ++ varname
>                in [$sqlQuote|
>
>   create table $(tablename) (
>    $(varname) $(typename)
>   );
>
>   create function $(fnname)() returns $(typename) as $a$
>     select * from $(tablename);
>   $a$ language sql stable;
>
>                    |] ++ tl
>         x1 -> x1
>
