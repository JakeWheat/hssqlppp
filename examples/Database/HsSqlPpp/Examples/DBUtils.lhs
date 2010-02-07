Copyright 2010 Jake Wheat

This file contains a few hacked together utility functions for working
with postgres.

> module Database.HsSqlPpp.Examples.DBUtils
>     (readCatalog
>     ,loadSqlUsingPsql
>     ,loadSqlUsingPsqlFromFile
>     ,clearDB
>     ,pgDump) where


> import System

> import System.Process.Pipe
> --import Text.Pandoc


> import Database.HsSqlPpp.Catalog

> import Database.HsSqlPpp.SqlTypes

> import Database.HsSqlPpp.Utils.DbmsCommon

> -- | get the catalog from the database, and return an Catalog value
> readCatalog :: String -> IO (Either [TypeError] Catalog)
> readCatalog dbName =
>   (readCatalogFromDatabase dbName) >>=
>     return . updateCatalog defaultCatalog

> -- | run psql to load the sql text into a database.
> loadSqlUsingPsql :: String -> String -> IO String
> loadSqlUsingPsql dbName =
>   pipeString [("psql", [dbName
>                        ,"-q"
>                        ,"--set"
>                        ,"ON_ERROR_STOP=on"
>                        ,"--file=-"])]

> -- | run psql to load sql from the filename given into a database.
> loadSqlUsingPsqlFromFile :: String -> FilePath -> IO (Either String String)
> loadSqlUsingPsqlFromFile dbName fn = do
>   ex <- system ("psql " ++ dbName ++
>                 " -q --set ON_ERROR_STOP=on" ++
>                 " --file=" ++ fn)
>   case ex of
>     ExitFailure e -> return $ Left $ "psql failed with " ++ show e
>     ExitSuccess -> return $ Right ""

> -- | use a dodgy hack to clear the database given
> clearDB :: String -> IO ()
> clearDB db =
>   withConn ("dbname=" ++ db) $ \conn ->
>     runSqlCommand conn "drop owned by jake cascade;"

> -- | dump the given database to sql source using pg_dump
> pgDump :: String -> IO String
> pgDump db = pipeString [("pg_dump", [db
>                                              ,"--schema-only"
>                                              ,"--no-owner"
>                                              ,"--no-privileges"])] ""

