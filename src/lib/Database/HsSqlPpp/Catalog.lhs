
Forward the public part of CatalogInternal.

> {- | This module contains the database catalog data types and helper
>  functions.
>
>  The catalog data type serves the following purposes:
>
>  * Contains all the catalog information needed to type check against
>     an existing database.
>
>  * A copy of the catalog information from a default template1
>    database is included - 'defaultTemplate1Catalog'.
>
>  * It is used internally to keep track of updates to the catalog
>     whilst running an annotation process (e.g. so that a select can
>     type check against a create table given in the same source). It
>     is also used to track other identifier types, such as attribute
>     references in select expressions, and argument and variable
>     types inside create function statements.
>
>  You can see what kind of stuff is contained in the Catalog type
>  by looking at the 'CatalogUpdate' type.
>
> -}
>
> module Database.HsSqlPpp.Catalog
>     (
>      -- * Data types
>      Catalog
>      -- ** Updates
>     ,CatalogUpdate(..)
>     ,ppCatUpdate
>      -- ** bits and pieces
>     ,CastContext(..)
>     ,CompositeFlavour(..)
>     ,CompositeDef
>     ,FunctionPrototype
>     ,DomainDefinition
>     ,FunFlav(..)
>      -- * Catalog values
>     ,emptyCatalog
>     ,defaultCatalog
>     ,defaultTemplate1Catalog
>      -- * Catalog comparison
>     ,CatalogDiff(..)
>     ,compareCatalogs
>     ,ppCatDiff
>      -- * Functions
>     ,updateCatalog
>     ,deconstructCatalog
>      -- * operator utils
>     ,OperatorType(..)
>     ,getOperatorType
>     ,isOperatorName
>     ) where
>
> import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal
> import Database.HsSqlPpp.AstInternals.Catalog.DefaultTemplate1Catalog
