
This file contains a few hacked together utility functions for working
with postgres.

> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Utils.DBUtils
>     (loadSqlUsingPsql
>     ,loadSqlUsingPsqlFromFile
>     ,clearDB
>     ,clearDBN
>     ,pgDump
>     ,readCatalog) where
>
> import System
> import System.Process.Pipe
>
> import Database.HsSqlPpp.Utils.DbmsCommon
> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.SqlTypes
>
>
> -- | run psql to load the sql text into a database.
> loadSqlUsingPsql :: String -> String -> IO String
> loadSqlUsingPsql dbName =
>   pipeString [("psql", [dbName
>                        ,"-q"
>                        ,"--set"
>                        ,"ON_ERROR_STOP=on"
>                        ,"--file=-"])]
>
> -- | run psql to load sql from the filename given into a database.
> loadSqlUsingPsqlFromFile :: String -> FilePath -> IO (Either String String)
> loadSqlUsingPsqlFromFile dbName fn = do
>   ex <- system ("psql " ++ dbName ++
>                 " -q --set ON_ERROR_STOP=on" ++
>                 " --file=" ++ fn)
>   case ex of
>     ExitFailure e -> return $ Left $ "psql failed with " ++ show e
>     ExitSuccess -> return $ Right ""
>
> -- | use a dodgy hack to clear the database given
> clearDB :: IConnection conn => conn -> IO ()
> clearDB conn = do
>   --withConn ("dbname=" ++ db) $ \conn -> do
>     --runSqlCommand conn "create language plpgsql;"
>     runSqlCommand conn "drop language if exists plpgsql cascade;"
>     runSqlCommand conn "create language plpgsql;"
>     runSqlCommand conn [$here|
\begin{code}
create function drop_all_user() returns void as $$
declare
  s text;
begin
  s := 'drop owned by ' || current_user || ' cascade;';
  execute s;
end;
$$ language plpgsql;
\end{code}
>                        |]
>     runSqlCommand conn "select drop_all_user();"
>     runSqlCommand conn "drop language if exists plpgsql cascade;"

> clearDBN :: String -> IO ()
> clearDBN db =
>   withConn ("dbname=" ++ db) clearDB

>
> -- | dump the given database to sql source using pg_dump
> pgDump :: String -> IO String
> pgDump db = pipeString [("pg_dump", [db
>                                     ,"--schema-only"
>                                     ,"--no-owner"
>                                     ,"--no-privileges"])] ""

> -- | get the catalog from the database, and return an Catalog value
> readCatalog :: String -> IO (Either [TypeError] Catalog)
> readCatalog dbName =
>   fmap (updateCatalog defaultCatalog)
>        (readCatalogFromDatabase dbName)
