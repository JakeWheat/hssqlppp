Copyright 2009 Jake Wheat

This module just forwards the public components of ScopeData,
ScopeReader and DefaultScope. Some of the exported functions in
ScopeData are only used by the type checker and are not fowarded.
Don't really know if using the word scope for this is correct English?

> {- | This module contains the scope data type and a few helper functions,
>  not really ready for public consumption yet. It serves the following purposes:
>
>  * contains all the catalog information needed to type check against an existing database
>
>  * a copy of the catalog information from a default template1
>     database is included - 'defaultScope', at some point this will allow typechecking
>     sql code against this catalog without having an available
>    postgresql install .
>
>  * it is also used internally in the type checker to hold identifiers
>     valid and their types in a given context, and to build the
>     additional catalog information so that e.g. a sql file containing
>     a create table followed by a select from that table will type
>     check ok. (This isn't quite working yet but it's almost there.)
> -}
> module Database.HsSqlPpp.TypeChecking.Scope
>     (
>     --fowarded scopedata
>      Scope(..)
>      ,QualifiedScope
>      ,emptyScope
>      ,combineScopes
>      ,readScope
>      ,defaultScope
> ) where

> import Database.HsSqlPpp.TypeChecking.ScopeData
> import Database.HsSqlPpp.TypeChecking.ScopeReader
> import Database.HsSqlPpp.TypeChecking.DefaultScope