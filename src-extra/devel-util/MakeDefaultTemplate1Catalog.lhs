
> {-# LANGUAGE QuasiQuotes #-}

> import Text.Groom

> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Utils.CatalogReader
> import Database.HsSqlPpp.Utils.PgUtils

> main :: IO ()
> main = do
>   let cs = "dbname=template1"
>   cus <- readCatalogFromDatabase cs
>   let s = groom cus
>   v <- withConn cs $ \conn -> do
>          r <- selectRelation conn "select version();" []
>          return (head $ head r)
>   putStrLn $ pre v ++ "\n" ++
>      unlines (map (">        " ++) $ lines s)


> pre :: String -> String
> pre v = [here|
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
> -- latest postgres. 'select version()' from the dbms this catalog
> -- was generated from: |] ++  v  ++ [here|
> defaultTemplate1Catalog :: Catalog
> defaultTemplate1Catalog =
>     (\l -> case l of
>              Left x -> error $ show x
>              Right e -> e) $
>      updateCatalog defaultCatalog
\end{code}
>   |]
