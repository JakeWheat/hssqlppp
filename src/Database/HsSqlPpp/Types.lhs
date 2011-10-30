
This is the public module to the SQL data types, mainly from TypeType.

> {- | Contains the SQL data types, type errors, and a few supporting
>      functions.
> -}
>
> module Database.HsSqlPpp.Types
>     (
>      -- * SQL types
>      Type (..)
>     ,PseudoType (..)
>      -- * type aliases
>     ,typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4
>     ,typeFloat8,typeVarChar,typeChar,typeBool,typeDate
>     ,typeInterval
>     ,canonicalizeTypeName
>     ,canonicalizeTypeNames
>      -- * Type errors
>     ,TypeError (..)
>     ) where
>
> import Database.HsSqlPpp.Internals.TypesInternal
