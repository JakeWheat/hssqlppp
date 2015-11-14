



> module Database.HsSqlPpp.Dialect
>     (Dialect(..)
>     ,SyntaxFlavour(..)
>     ,ansiDialect
>     ,postgresDialect
>     ,sqlServerDialect
>     ,oracleDialect
>     ,canonicalizeTypeName
>     ,ansiTypeNameToDialect
>     ) where

> import Database.HsSqlPpp.Internals.Dialect

> import Database.HsSqlPpp.Dialects.Ansi
> import Database.HsSqlPpp.Dialects.Postgres
> import Database.HsSqlPpp.Dialects.Oracle
> import Database.HsSqlPpp.Dialects.SqlServer
