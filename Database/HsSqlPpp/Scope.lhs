Copyright 2009 Jake Wheat

This module just forwards the public components of ScopeData,
ScopeReader and DefaultScope. Some of the exported functions in
ScopeData are only used by the type checker and are not fowarded.

> module Database.HsSqlPpp.Scope
>     (
>     --fowarded scopedata
>      Scope(..)
>      ,QualifiedScope
>      ,emptyScope
>      ,combineScopes
>      --forwarded scopereader
>      ,readScope
>      --forwarded defaultscope
>      ,defaultScope
> ) where

> import Database.HsSqlPpp.ScopeData
> import Database.HsSqlPpp.ScopeReader
> import Database.HsSqlPpp.DefaultScope