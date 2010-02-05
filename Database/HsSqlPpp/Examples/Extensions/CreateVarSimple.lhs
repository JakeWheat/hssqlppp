Copyright 2010 Jake Wheat

Demonstration extension: createVar
==================================

Extension to remove the boilerplate from adding tables with a single
attribute and single row, a bit like a global variable in the
database.

See [ExtensionsUtils](ExtensionsUtils.lhs.html) for the support types
and functions.

> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.Examples.Extensions.CreateVarSimple
>     where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils

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

Target SQL template
-------------------

Create a template ast with placeholders from literal SQL, then run a
transform on this ast, replacing the placeholders with the correct
values for each specific create_var call. Robbed this idea from the
HList source code, don't know how widely used it is.

> createVarTemplate :: [Statement]
> createVarTemplate = readTemplate
>   [$here|
>
>   create table createvarvarname_table (
>     createvarvarname createvarvartype
>   );
>
>   create function get_createvarvarname() returns createvarvartype as $a$
>     select * from createvarvarname_table;
>   $a$ language sql stable;
>
>   |]

For createVarSimple, we only need to replace the identifiers -
createvarvarname, createvarvartype, createvarvarname_table and
get_createvarvarname.

The ast for this SQL for reference:

~~~~{.Haskell}
[CreateTable [] "createvarvarname_table"
   [AttributeDef [] "createvarvarname"
      (SimpleTypeName [] "createvarvartype")
      Nothing
      []]
   [],
 CreateFunction [] "get_createvarvarname" []
   (SimpleTypeName [] "createvarvartype")
   Sql
   "$a$"
   (SqlFnBody []
      [SelectStatement []
         (Select [] Dupes (SelectList [] [SelExp [] (Identifier [] "*")] [])
            [Tref [] "createvarvarname_table" NoAlias]
            Nothing
            []
            Nothing
            []
            Nothing
            Nothing)])
   Stable]
~~~~

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
>                                     [StringLit _ _ tableName
>                                     ,StringLit _ _ typeName]):tl

Create the replacement Ast. replaceSqlStrings replaces any strings
anywhere in the Ast which match.

>             -> replaceSqlStrings [("createvarvarname_table",
>                                    tableName ++ "_table")
>                                  ,("createvarvarname", tableName)
>                                  ,("createvarvartype", typeName)
>                                  ,("get_createvarvarname",
>                                    "get_" ++ typeName)]
>                  createVarTemplate
>                ++ tl
>         x1 -> x1
>
