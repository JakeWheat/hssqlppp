



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

> import Database.HsSqlPpp.Internals.Dialects.Ansi
> import Database.HsSqlPpp.Internals.Dialects.Postgres
> import Database.HsSqlPpp.Internals.Dialects.Oracle
> import Database.HsSqlPpp.Internals.Dialects.SqlServer
