Copyright 2009 Jake Wheat

> module Database.HsSqlPpp.Ast.Annotation
>     (
>      -- * Annotation data types
>      Annotation
>     ,AnnotationElement(..)
>      -- * SQL types
>     ,Type (..)
>     ,PseudoType (..)
>      -- * type aliases
>      -- | aliases for all the sql types with multiple names
>      -- these give you the canonical names
>     ,typeSmallInt,typeBigInt,typeInt,typeNumeric,typeFloat4
>     ,typeFloat8,typeVarChar,typeChar,typeBool
>      -- * Type errors
>     ,TypeError (..)
>      -- * Statement info
>      -- | This is the main annotation attached to each statement. Early days at the moment
>      -- but will be expanded to provide any type errors lurking inside a statement, any useful
>      -- types, e.g. the types of each select and subselect/sub query in a statement,
>      -- any changes to the catalog the statement makes, and possibly much more information.
>     ,StatementInfo(..)
>     ,stripAnnotations
>     ) where

> import Database.HsSqlPpp.AstInternals.AstAnnotation
> import Database.HsSqlPpp.AstInternals.TypeType
