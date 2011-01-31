
Forward the public part of ParserInternal

> -- | Functions to parse SQL.
> module Database.HsSqlPpp.Parser (
>              -- * Main
>               parseStatements
>              ,parseStatementsWithPosition
>              ,parseStatementsFromFile
>              ,parseQueryExpr
>              -- * Testing
>              ,parseScalarExpr
>              ,parsePlpgsql
>              -- * errors
>              ,ParseErrorExtra(..)
>              )
>     where
> import Database.HsSqlPpp.Parsing.ParserInternal