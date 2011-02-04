
This is the public module to the SQL data types, mainly from TypeType.

> {- | Contains the SQL data types, type errors, and a few supporting
>      functions.
> -}
>
> module Database.HsSqlPpp.SqlTypes
>     (
>      -- * SQL types
>      Type (..)
>     ,PseudoType (..)
>      -- * type aliases
>      -- | aliases for all the sql types with multiple names
>      -- these give you the canonical names
>     ,typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4
>     ,typeFloat8,typeVarChar,typeChar,typeBool,typeDate
>     ,canonicalizeTypeName
>     ,canonicalizeTypes
>      -- * Type errors
>     ,TypeError (..)
>     ) where
>
> import Database.HsSqlPpp.AstInternals.TypeType
