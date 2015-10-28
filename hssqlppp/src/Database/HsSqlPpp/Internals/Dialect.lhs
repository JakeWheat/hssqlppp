
> module Database.HsSqlPpp.Internals.Dialect where

> -- | The dialect of SQL to use for parsing, pretting printing or
> -- typechecking.
> data Dialect = PostgreSQL
>              | SQLServer
>              | Oracle
>              deriving (Show,Eq)
