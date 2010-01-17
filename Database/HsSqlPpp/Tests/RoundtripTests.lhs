Copyright 2010 Jake Wheat

Test sql by typechecking it, then running it through Postgres and comparing.

> module Database.HsSqlPpp.Tests.RoundtripTests (roundtripTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Control.Monad.Error
> import Data.List
> import Data.Generics
> import Data.Generics.PlateData
> import Data.Char

> import Database.HsSqlPpp.Utils
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Catalog
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Dbms.DBUtils
> import Database.HsSqlPpp.Dbms.DatabaseLoader
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Ast.Ast
> import Database.HsSqlPpp.Ast.SqlTypes

slightly dodgy, these tests automatically connect to this database and
clear it. hopefully no-one running these tests is storing important
data in a database with this name

> testDatabaseName :: String
> testDatabaseName = "hssqlpppautomatedtests"

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
>      ,("create domain with check"
>       ,"create domain testd as text check (length(value) > 2);")
>      ,("create composite"
>       ,"create type pos as (\n\
>        \  x int,\n\
>        \  y int);")

>     ]]

> itemToTft :: Item -> [Test.Framework.Test]
> itemToTft (Group s is) = [testGroup s $ concatMap itemToTft is]
> itemToTft (Src ss) = map (uncurry testRoundtrip) ss

> testRoundtrip :: String -> String -> Test.Framework.Test
> testRoundtrip name sql = testCase ("test " ++ name) $
>   checkStage1 >>
>   checkStage2 >>
>   checkStage3
>   where

Stage 1:
read sql files: produce catalog using type checker
load sql files into postgres using psql
read catalog from psql and compare with catalog from typechecker

>     checkStage1 = wrapETT $ do
>       ast <- tsl $ parseSql "" sql
>       let (cat1,tast) = typeCheck defaultTemplate1Catalog ast
>       failIfTypeErrors tast
>       liftIO $ clearDB testDatabaseName
>       liftIO $ loadSqlUsingPsql testDatabaseName sql
>       dbCat <- liftIO (readCatalog testDatabaseName) >>= tsl
>       case compareCatalogs defaultTemplate1Catalog cat1 dbCat of
>               CatalogDiff [] [] -> liftIO $ return ()
>               c -> liftIO $ assertFailure $ "catalogs difference: " ++ ppCatDiff c

stage 2:
as above, but load via databaseloader

>     checkStage2 = wrapETT $ do
>       ast <- tsl $ parseSql "" sql
>       let (cat1,_) = typeCheck defaultTemplate1Catalog ast
>       liftIO $ clearDB testDatabaseName
>       liftIO $ loadIntoDatabase testDatabaseName "" ast
>       dbCat <- liftIO (readCatalog testDatabaseName) >>= tsl
>       case compareCatalogs defaultTemplate1Catalog cat1 dbCat of
>               CatalogDiff [] [] -> liftIO $ return ()
>               c -> liftIO $ assertFailure $ "catalogs difference: " ++ ppCatDiff c


stage 3:
load using database loader
dump using pg_dump
attempt to compare original ast to ast of dump
compare catalogs

>     checkStage3 = wrapETT $ do
>       ast <- tsl $ parseSql "" sql
>       let cat = fst $ typeCheck defaultTemplate1Catalog ast
>       dump <- liftIO $ pgDump testDatabaseName
>       nast <- tsl $ parseSql "" dump
>       let (ncat, tnast) = typeCheck defaultTemplate1Catalog nast
>       failIfTypeErrors $ tnast
>       case compareCatalogs defaultTemplate1Catalog cat ncat of
>               CatalogDiff [] [] -> liftIO $ return ()
>               c -> liftIO $ assertFailure $ "catalogs difference: " ++ ppCatDiff c
>       let past = adjustAstToLookLikeDump $ adjTree ast
>           pnast = adjTree nast
>       when (past /= pnast) $
>         liftIO $ putStrLn $ sql ++ "\n" ++ dump
>       liftIO $ assertEqual "check dump ast" past pnast
>     adjTree = canonicalizeTypeNames . stripAnnotations
>     failIfTypeErrors xast = do
>       let te = getTypeErrors xast
>       when (not $ null te) $ throwError $ show te


take the parse tree and change the type names to the canonical versions

> canonicalizeTypeNames :: Data a => a -> a
> canonicalizeTypeNames =
>   transformBi $ \x ->
>       case x of
>         SimpleTypeName a tn -> SimpleTypeName a $ canonicalizeTypeName tn
>         x1 -> x1


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


> adjustAstToLookLikeDump :: [Statement] -> [Statement]
> adjustAstToLookLikeDump ast =
>   ((presets ++) . stripDml . addConstraintNames) ast
>   where
>     -- add the following at the beginning of the ast, since this is what pg_dump does
>     -- SET statement_timeout = 0;
>     -- SET client_encoding = 'UTF8';
>     -- SET standard_conforming_strings = off;
>     -- SET check_function_bodies = false;
>     -- SET client_min_messages = warning;
>     -- SET escape_string_warning = off;
>
>     -- SET search_path = public, pg_catalog;
>     noDml = stripDml ast
>     presets = [Set [] "statement_timeout" [SetNum [] 0.0]
>               ,Set [] "client_encoding" [SetStr [] "UTF8"]
>               ,Set [] "standard_conforming_strings" [SetId [] "off"]
>               ,Set [] "check_function_bodies" [SetId [] "false"]
>               ,Set [] "client_min_messages" [SetId [] "warning"]
>               ,Set [] "escape_string_warning" [SetId [] "off"]]
>               -- if there are no statements, pg_dump doesn't spit out the search path
>               ++ if null noDml then []  else
>                      [Set [] "search_path" [SetId [] "public", SetId [] "pg_catalog"]]
>     -- dml statements don't appear in the dump
>     stripDml = filter (\s -> case s of
>                                SelectStatement _ _ -> False
>                                Insert _ _ _ _ _ -> False
>                                Update _ _ _ _ _ -> False
>                                Delete _ _ _ _ -> False
>                                Copy _ _ _ _ -> False
>                                CopyData _ _ -> False
>                                Truncate _ _ _ _ -> False
>                                _ -> True)

when pg comes across a constraint without a name, it generates one
automatically and this appears in the dump, so try to follow the
generation here, also any 'value' identifiers will be in uppercase

> addConstraintNames :: Data a => a -> a
> addConstraintNames =
>   transformBi $ \x ->
>       case x of
>         CreateDomain a name base "" cons ->
>             CreateDomain a name base
>               (case cons of
>                          Nothing -> ""
>                          Just _ -> name ++ "_check") (upcaseValue cons)
>                   where
>                     upcaseValue = transformBi $ \y ->
>                                   case y of
>                                     Identifier a1 i | map toUpper i == "VALUE" ->
>                                           Identifier a1 "VALUE"
>                                     y1 -> y1
>         x1 -> x1

================================================================================

some random support functions, to be tidied up

> wrapETT :: (Show e) => ErrorT e IO () -> IO ()
> wrapETT c = runErrorT c >>= \x ->
>          case x of
>            Left er -> assertFailure $ show er
>            Right l -> return l
