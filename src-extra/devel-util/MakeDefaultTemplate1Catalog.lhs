
> {-# LANGUAGE QuasiQuotes #-}

> import Text.Groom

> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Utils.CatalogReader

> main :: IO ()
> main = do
>   cus <- readCatalogFromDatabase "dbname=template1"
>   let s = groom cus
>   putStrLn $ pre ++ "\n" ++
>      unlines (map (">        " ++) $ lines s)


> pre :: String
> pre = [here|
\begin{code}

This file is auto generated, to regenerate use the
regenDefaultTemplate1catalog.sh script. You will need postgresql
installed to do this.

> module Database.HsSqlPpp.Internals.Catalog.DefaultTemplate1Catalog
>      (defaultTemplate1Catalog) where
>
> import Database.HsSqlPpp.Internals.Catalog.CatalogInternal
> --import Database.HsSqlPpp.Internals.TypesInternal
> -- | The catalog from a default template1 database in roughly the
> -- latest postgres. (At some point will provide catalogs from different
> -- versions, the one in this release is probably from 9.1.1)
> defaultTemplate1Catalog :: Catalog
> defaultTemplate1Catalog =
>     (\l -> case l of
>              Left x -> error $ show x
>              Right e -> e) $
>      updateCatalog defaultCatalog
\end{code}
>   |]
