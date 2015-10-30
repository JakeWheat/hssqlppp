
> module Database.HsSqlPpp.Internals.Dialect where

> -- | The dialect of SQL to use for parsing, pretting printing or
> -- typechecking.
> data Dialect = ANSI
>              | PostgreSQL
>              | SQLServer
>              | Oracle
>              deriving (Show,Eq)

dialects to consider adding:
db2, teradata, mysql, ...

Dialect todo:

have to figure out what goes in the dialect and what goes in the
catalog. Maybe the catalog should be part of the dialect?


Choices:

type aliases
schema search path behaviour
schema for internal stuff
which builtins/special syntax supported

types for literals: can use the unknown style, or type them as strings
or numbers

what kind of cases to support (e.g. don't allow multi test when
branches)

positionalarg,placeholder,host parameter
extended aggregates
window support
subquery variations
odbc
arrays, multisets
collations
next value for

what kind of things are in ansi but not all products support?
