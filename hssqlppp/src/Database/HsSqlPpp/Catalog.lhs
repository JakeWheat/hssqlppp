
Forward the public part of CatalogInternal.

> {- | This module contains the database catalog data types and helper
>  functions.
>
> The catalog data type holds the catalog information needed to type
> check sql code, and a catalog value is produced after typechecking sql
> which represents the catalog that would be produced (e.g. for sql
> containing ddl)
>

> You can create a catalog using the 'CatalogUpdate' type, and there
> is example and util in the repo which reads a catalog from
> an existing database in postgres.
>
> -}
>
> module Database.HsSqlPpp.Catalog
>     (
>      -- * Data types
>      Catalog
>      -- ** Updates
>     ,CatalogUpdate(..)
>     --,ppCatUpdate
>      -- ** bits and pieces
>     ,CastContext(..)
>     ,CompositeFlavour(..)
>     ,CatName
>     ,CatNameExtra(..)
>     ,mkCatNameExtra
>     ,mkCatNameExtraNN
>     --,CompositeDef
>     --,FunctionPrototype
>     --,DomainDefinition
>     --,FunFlav(..)
>     --  --  * Catalog values
>     --,emptyCatalog
>     --,defaultCatalog
>     --,ansiCatalog
>     --,defaultTemplate1Catalog
>     --,defaultTSQLCatalog
>     --,odbcCatalog
>      -- -- * Catalog comparison
>     --,CatalogDiff(..)
>     --,compareCatalogs
>     --,ppCatDiff
>      -- * Functions
>     ,updateCatalog
>     ,deconstructCatalog
>      -- * testing support
>     ,Environment
>     ,brokeEnvironment
>     ,envSelectListEnvironment
>     ) where
>
> import Database.HsSqlPpp.Internals.Catalog.CatalogBuilder
> import Database.HsSqlPpp.Internals.Catalog.CatalogTypes
> --import Database.HsSqlPpp.Internals.Catalog.DefaultTemplate1Catalog
> --import Database.HsSqlPpp.Internals.Catalog.DefaultTSQLCatalog
> --import Database.HsSqlPpp.Internals.Catalog.OdbcCatalog
> --import Database.HsSqlPpp.Internals.Catalog.AnsiCatalog
> import Database.HsSqlPpp.Internals.TypeChecking.Environment
