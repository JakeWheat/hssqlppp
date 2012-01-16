
> -- | Hacky start on a separate catalog for tsql. At the moment, reuses the
> -- postgresql default template1 catalog and adds a few things.
> module Database.HsSqlPpp.Internals.Catalog.DefaultTSQLCatalog
>      (defaultTSQLCatalog) where
>
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> import Database.HsSqlPpp.Internals.Catalog.DefaultTemplate1Catalog

> defaultTSQLCatalog :: Catalog
> defaultTSQLCatalog =
>     (\l -> case l of
>              Left x -> error $ show x
>              Right e -> e) $
>      flip updateCatalog defaultTemplate1Catalog

>     [CatCreateBinaryOp "+" "varchar" "varchar" "varchar"
>     ,CatCreateFunction "getdate" [] False "date"
>     ,CatCreateFunction "isnumeric" ["anyelement"] False "int4"
>     ,CatCreateFunction "grt_lengthconv" ["int4"] False "int4"
>     ,CatCreateFunction "isnull" ["anyelement","anyelement"] False "anyelement"
>     -- put these in to stop use the text only version and a bunch of casts
>     ,CatCreateFunction "replace" ["char", "char", "char"] False "char"
>     ,CatCreateFunction "replace" ["varchar", "varchar", "varchar"] False "varchar"
>     ]


