> module Database.HsSqlPpp.AstInternals.Catalog.DefaultTemplate1Catalog
>     (defaultTemplate1Catalog
>      ) where
>
> import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal
> import Database.HsSqlPpp.AstInternals.TypeType
>
> defaultTemplate1Catalog :: Catalog
> defaultTemplate1Catalog =
>    (\l -> case l of
>             Left x -> error $ show x
>             Right e -> e) $
>     updateCatalog defaultCatalog
>        []

