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
> import Database.HsSqlPpp.Ast.Catalog
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Dbms.DBUtils

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
>       let (cat1,_) = typeCheck defaultTemplate1Catalog ast
>       liftIO $ clearDB "testing"
>       liftIO $ loadSqlUsingPsql "testing" sql
>       dbCat <- liftIO (readCatalog "testing") >>= tsl
>       case compareCatalogs defaultTemplate1Catalog cat1 dbCat of
>               CatalogDiff [] [] -> liftIO $ return ()
>               c -> liftIO $ assertFailure $ "catalogs difference: " ++ ppCatDiff c

================================================================================

ast roundtrip tests:
want to compare the asts of parsed sql, with the asts of the sql
loaded into pg, dumped and parsed
issues:
some statements are split apart e.g. create table with constraints
some statements are reordered in the dump
a create, select and then drop will be a problem
so need some sort of map function to convert between, and use
extensions to cover the dynamic ddl


one of the things really want to double check is associativity and
precedence mainly in select expressions, pg_dump puts in the implicit
brackets which we can use to check these things


================================================================================

some random support functions, to be tidied up
find any that can be shared with hssqlsystem and move to common module or something

> wrapETT :: (Show e) => ErrorT e IO () -> IO ()
> wrapETT c = runErrorT c >>= \x ->
>          case x of
>            Left er -> assertFailure $ show er
>            Right l -> return l

> {- -- | use a dodgy hack to clear the database given
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
>                        ,"--file=-"])] -}

