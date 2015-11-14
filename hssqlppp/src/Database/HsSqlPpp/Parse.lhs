
Forward the public part of ParserInternal

> -- | Functions to parse SQL.

> module Database.HsSqlPpp.Parse
>     (-- * Main
>      parseStatements
>     ,parseProcSQL
>     ,parseQueryExpr
>     ,parseScalarExpr
>      -- * Parsing options
>     ,ParseFlags(..)
>     ,defaultParseFlags
>     ,Dialect(..)
>     ,ansiDialect
>      -- * errors
>     ,ParseErrorExtra(..)
>      -- * internals
>     ,parseName
>     ,parseNameComponent

>     ) where
>
> import Database.HsSqlPpp.Internals.ParseInternal
> import Database.HsSqlPpp.Internals.ParseErrors
> import Database.HsSqlPpp.Dialects.Ansi
> import Database.HsSqlPpp.Internals.Dialect

> defaultParseFlags :: ParseFlags
> defaultParseFlags = ParseFlags {pfDialect = ansiDialect}
