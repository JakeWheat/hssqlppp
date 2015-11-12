
This is the public module to the SQL data types, mainly from TypeType.

> {- | Contains the SQL data types, type errors, and a few supporting
>      functions.
> -}
>
> module Database.HsSqlPpp.Types
>     (
>      -- * SQL types
>      Type (..)
>     ,TypeExtra (..)
>     ,PseudoType (..)
>     {-,typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4
>     ,typeFloat8,typeVarChar,typeChar,typeBool,typeDate
>     ,typeTime,typeTimestamp,typeInterval-}
>      -- * Type errors
>     ,TypeError (..)
>     ) where
>
> import Database.HsSqlPpp.Internals.TypesInternal
