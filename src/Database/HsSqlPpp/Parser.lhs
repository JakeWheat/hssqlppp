Copyright 2010 Jake Wheat

Forward the public part of ParserInternal

> -- | Functions to parse SQL.
> module Database.HsSqlPpp.Parser (
>              -- * Main
>               parseSql
>              ,parseSqlWithPosition
>              ,parseSqlFile
>              -- * Testing
>              ,parseExpression
>              ,parsePlpgsql
>              -- * errors
>              ,ParseErrorExtra(..)
>              )
>     where
> import Database.HsSqlPpp.Parsing.ParserInternal