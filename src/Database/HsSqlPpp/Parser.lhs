
Forward the public part of ParserInternal

> -- | Functions to parse SQL. Can parse from String or
> -- Lazy/Strict ByteString and Text.

> module Database.HsSqlPpp.Parser
>     (-- * Main
>      parseStatements
>     ,parseQueryExpr
>     ,parseScalarExpr
>     ,parsePlpgsql
>      -- * Parsing options
>     ,ParseFlags(..)
>     ,defaultParseFlags
>     ,SQLSyntaxDialect(..)
>      -- * errors
>     ,ParseErrorExtra(..)
>     ) where
>
> import Database.HsSqlPpp.Parsing.ParserInternal
