
See the odbcparsing for an overview.

1. date, time, and timestamp literals: these are trivially type
checked (and we don't check the format of the literal string
in hssqlppp currently)
2. scalar function calls: see below
3. escapes for like strings: not currently supported
4. outer joins: works like trefparens
5. calling a procedure: not currently supported

scalar functions

if we write:
select _f_(a), {fn _g_(b)} from t

the _f_ function is looked up in the usual catalog
but the _g_ function is looked up in an alternative namespace for
functions. We can't combine these two namespaces because the valid
functions are different in each namespace.

So we have to create a special case in the catalog and type checking
code. Since odbc is old, this is considered a fixed list and is just
implemented as a separate list of function prototypes to check against
when you are directly inside a {fn ... } node.

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.OdbcTypechecking (odbcTypechecking) where
>
> import Database.HsSqlPpp.Ast

> import Database.HsSqlPpp.Tests.Parsing.Utils
> import Database.HsSqlPpp.Tests.TestTypes

> odbcTypechecking:: Item
> odbcTypechecking =
>    Group "odbcTypechecking" [
>    ]
