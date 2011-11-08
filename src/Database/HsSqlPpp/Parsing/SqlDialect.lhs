

> module Database.HsSqlPpp.Parsing.SqlDialect where

> -- | The dialect of SQL to use. More options to be added
> data SQLSyntaxDialect = PostgreSQLDialect
>                       | SQLServerDialect
>                         deriving (Show,Eq)
