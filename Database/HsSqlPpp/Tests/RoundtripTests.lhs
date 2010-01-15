Copyright 2010 Jake Wheat

Test sql by typechecking it, then running it through Postgres and comparing.

Stage 1:
read sql files: produce catalog using type checker
load sql files into postgres using psql
read catalog from psql and compare with catalog from typechecker
stage 2:
as above, but load via databaseloader
stage 3:
load using database loader
dump using pg_dump
attempt to compare original ast to ast of dump


> module Database.HsSqlPpp.Tests.RoundtripTests (roundtripTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Control.Monad.Error
> import Data.List

> import Database.HsSqlPpp.Utils
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Environment
> import Database.HsSqlPpp.Ast.SqlTypes
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Dbms.DBAccess
> import System.Process.Pipe

> data Item = Group String [Item]
>           | Src [(String,String)]

> roundtripTests :: [Test.Framework.Test]
> roundtripTests = itemToTft roundtripTestData

> roundtripTestData :: Item
> roundtripTestData =
>     Group "round trip tests" [ Src [
>       ("simple select"
>       ,"select 1 from pg_attrdef;")
>      ,("create domain"
>       ,"create domain testd as text;")

>     ]]

> itemToTft :: Item -> [Test.Framework.Test]
> itemToTft (Group s is) = [testGroup s $ concatMap itemToTft is]
> itemToTft (Src ss) = map (uncurry testRoundtrip) ss

> testRoundtrip :: String -> String -> Test.Framework.Test
> testRoundtrip name sql = testCase ("test " ++ name) $
>   checkStage1
>   where

Stage 1:
read sql files: produce catalog using type checker
load sql files into postgres using psql
read catalog from psql and compare with catalog from typechecker

>     checkStage1 = wrapETT $ do
>       ast <- tsl $ parseSql "" sql
>       let (cat1,_) = typeCheck defaultTemplate1Environment ast
>       liftIO $ clearDB "testing"
>       liftIO $ loadSqlUsingPsql "testing" sql
>       dbCat <- liftIO (readCatalog "testing") >>= tsl
>       case compareCatalogs defaultTemplate1Environment cat1 dbCat of
>               CatalogDiff [] [] -> liftIO $ return () --assertFailure "ok"
>               c -> liftIO $ assertFailure $ "catalogs difference: " ++ ppCatDiff c

> wrapETT :: (Show e) => ErrorT e IO () -> IO ()
> wrapETT c = runErrorT c >>= \x ->
>          case x of
>            Left er -> assertFailure $ show er
>            Right l -> return l


> -- | print a catdiff in a more human readable way than show.
> ppCatDiff :: CatalogDiff -> String
> ppCatDiff (CatalogDiff missing extr) =
>     "\nmissing:\n"
>     ++ intercalate "\n" (map ppEnvUpdate missing)
>     ++ "\nextra:\n"
>     ++ intercalate "\n" (map ppEnvUpdate extr)


> -- | use a dodgy hack to clear the database given
> clearDB :: String -> IO ()
> clearDB db = withConn ("dbname=" ++ db) $ \conn ->
>              runSqlCommand conn "drop owned by jake cascade;"

> -- | dump the given database to sql source using pg_dump
> pgDump :: String -> IO String
> pgDump db = pipeString [("pg_dump", [db
>                                     ,"--schema-only"
>                                     ,"--no-owner"
>                                     ,"--no-privileges"])] ""

> -- | get the catalog from the database
> readCatalog :: String -> IO (Either [TypeError] Environment)
> readCatalog dbName =
>   (readEnvironmentFromDatabase dbName) >>=
>     return . updateEnvironment defaultEnvironment

> -- | run psql to load the sql text into a database.
> loadSqlUsingPsql :: String -> String -> IO String
> loadSqlUsingPsql dbName =
>   pipeString [("psql", [dbName
>                        ,"-q"
>                        ,"--set"
>                        ,"ON_ERROR_STOP=on"
>                        ,"--file=-"])]

