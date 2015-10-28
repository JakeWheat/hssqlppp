
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
>      -- * errors
>     ,ParseErrorExtra(..)
>      -- * internals

TODO - export these from another module
will need lots more to support the extensions/chaos stuff

>     ,parseName
>     ,parseNameComponent

>     ) where
>
> import Database.HsSqlPpp.Internals.ParseInternal
