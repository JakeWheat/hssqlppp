
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

>     [-- temporary partial work around for different
>      -- implicit cast rules in tsql
>      CatCreateBinaryOp ">=" "varchar" "int4" "bool"
>     ,CatCreateBinaryOp ">" "varchar" "int4" "bool"
>     ,CatCreateBinaryOp "<" "varchar" "int4" "bool"
>     ,CatCreateBinaryOp "+" "varchar" "varchar" "varchar"
>     ,CatCreateBinaryOp "=" "int4" "varchar" "bool"

>     ,CatCreateFunction "getdate" [] False "date"
>     ,CatCreateFunction "isnumeric" ["anyelement"] False "int4"
>     ,CatCreateFunction "grt_lengthconv" ["int4"] False "int4"
>     ,CatCreateFunction "isnull" ["anyelement","anyelement"] False "anyelement"
>     ]


