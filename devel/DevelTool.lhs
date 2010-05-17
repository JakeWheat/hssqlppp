Make the website,

To compile, use something like:

time ghc -threaded -XDeriveDataTypeable -DPOSTGRES -cpp -pgmPcpphs -optP--cpp -idevel:src:examples/chaos:examples/extensions/:examples/util/:tests/ --make devel/DevelTool


> import System.Environment

> import MakeWebsite
> import MakeAntiNodes

> main :: IO ()
> main = do
>   args <- getArgs
>   case args of
>     ["sourcelinks"] -> sourceLinks
>     ["makewebsite"] -> makeWebsite
>     ["makeantinodes"] -> writeAntiNodes
>     x -> error $ "don't understand " ++ show x

-------------------------------------------------------------------------------

dbCatalog
=========

This reads an catalog from a database and writes it out using
show. Warning: these things are big since they contain all the default
PostgreSQL catalog contents.

> {-readCat :: String -> IO ()
> readCat dbName = do
>   cat <- readCatalogFromDatabase dbName
>   putStrLn preamble
>   putStrLn $ prefixLines $ ppExpr cat
>   where
>     preamble = [$here|

\begin{code}

This file is auto generated, to regenerate run
example/HsSqlSystem dbcatalog --database=template1 > src/Database/HsSqlPpp/AstInternals/Catalog/DefaultTemplate1Catalog.lhs

from the project root (i.e. where the cabal file is located).

> module Database.HsSqlPpp.AstInternals.Catalog.DefaultTemplate1Catalog
>      (defaultTemplate1Catalog) where
>
> import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal
> import Database.HsSqlPpp.AstInternals.TypeType
>
> defaultTemplate1Catalog :: Catalog
> defaultTemplate1Catalog =
>     (\l -> case l of
>              Left x -> error $ show x
>              Right e -> e) $
>      updateCatalog defaultCatalog
\end{code}
>|]
>     prefixLines = unlines . map (">        " ++) . lines -}
