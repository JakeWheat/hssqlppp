Copyright 2010 Jake Wheat

Test sql by typechecking it, then running it through Postgres and comparing:

* compare the catalog from typechecking to the one read from postgres
* load then dump the sql and compare post and pre asts
* (NOT STARTED ON YET) create views and check the type from type
  checking to the one from pg

> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Tests.RoundtripTests (roundtripTests) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Control.Monad.Error
> import Data.List
> import Data.Generics
> import Data.Generics.PlateData
> import Data.Char
>
> import Database.HsSqlPpp.Utils.Utils
> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Examples.DBUtils
> import Database.HsSqlPpp.Examples.DatabaseLoader
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.SqlTypes

slightly dodgy, these tests automatically connect to this database and
clear it. hopefully no-one running these tests is storing important
data in a database with this name

> testDatabaseName :: String
> testDatabaseName = "hssqlpppautomatedtests"
>
> data Item = Group String [Item]
>           | Src [(String,String)]
>
> roundtripTests :: [Test.Framework.Test]
> roundtripTests = itemToTft roundtripTestData
>
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
>      ,("create table"
>       ,"create table ttable (\n\
>        \  x int,\n\
>        \  y int);")
>      --,("create table with constraints"
>      -- ,"create table ttable (\n\
>      --  \  x int primary key,\n\
>      --  \  y int not null);")
>      ,("create view"
>       ,"create view v1 as select * from pg_attrdef;")
>      ,("create function"
>       ,"create function test1() returns integer as $$\n\
>        \  select 1;\n\
>        \$$ language sql;")
>      ,("create plpgsql function"
>       ,"create language plpgsql;\n\
>        \create function test1() returns void as $$\n\
>        \begin\n\
>        \  null;\n\
>        \end;\n\
>        \$$ language plpgsql;")
>      ,("join associativity"
>       ,"select * from pg_enum full outer join pg_largeobject on true full outer join pg_listener on true;")
>      ,("with and union associativity"
>       ,[$here|
>         create view v1 as
>         with
>           a as (select 1)
>          ,b as (select 2)
>          select * from a
>          union select * from b;|])
>     ]]

~~~~
TODO for test data:
run through constraints in create table after attribute and as seperate rows
do multiple constraints on a line
use create view to run through select variations
~~~~

--------------------------------------------------------------------------------

> itemToTft :: Item -> [Test.Framework.Test]
> itemToTft (Group s is) = [testGroup s $ concatMap itemToTft is]
> itemToTft (Src ss) = map (uncurry testRoundtrip) ss
>
> testRoundtrip :: String -> String -> Test.Framework.Test
> testRoundtrip name sql = testCase ("test " ++ name) $ wrapETT $ do
>   astOrig <- tsl $ parseSql "" sql
>   let (catOrig, astOrigTC) = typeCheck defaultTemplate1Catalog astOrig
>   failIfTypeErrors astOrigTC
>   -- run the tests first using psql to load the sql into the database
>   -- and then using hssqlppp's database loader to load the sql into
>   -- the database
>   doPgTests astOrig catOrig (liftIO (loadSqlUsingPsql testDatabaseName sql >> return ()))
>   doPgTests astOrig catOrig (liftIO $ loadAst testDatabaseName astOrig)
>   where
>     doPgTests :: [Statement] -> Catalog -> ErrorT String IO () -> ErrorT String IO () -- a -> a
>     doPgTests astOrig catOrig loadIntoDb = do
>       -- parse and type check the test sql
>       -- load this sql into pg
>       liftIO $ clearDB testDatabaseName
>       loadIntoDb
>       -- check the catalog in pg is the same as the one from type checking
>       catPsql <- liftIO (readCatalog testDatabaseName) >>= tsl
>       compareCats "load" catOrig catPsql
>       -- dump the database to get the sql having been normalized by passing
>       -- it through pg's digestive system
>       dumpSql <- liftIO $ pgDump testDatabaseName
>       astDumped <- tsl $ parseSql "" dumpSql
>       let (catDumped, astDumpedTC) = typeCheck defaultTemplate1Catalog astDumped
>       failIfTypeErrors $ astDumpedTC
>       -- check the original catalog from the catalog gotten from
>       -- dumping then typechecking the dump, maybe a little excessive
>       compareCats "dump" catOrig catDumped
>       -- compare the original ast to the dump ast, uses a transform
>       -- to match the changes that happen to the sql when loaded
>       -- then dumped by pg
>       let astOrigAdj = adjustAstToLookLikeDump $ adjTree astOrig
>           astDumpedAdj = adjTree astDumped
>       -- do this when a test fails to help diagnose why
>       when (astOrigAdj /= astDumpedAdj) $
>             liftIO $ putStrLn $ sql ++ "\n" ++ dumpSql
>       liftIO $ assertEqual "check dump ast" astOrigAdj astDumpedAdj
>
>     compareCats s c1 c2 =
>       case compareCatalogs defaultTemplate1Catalog c1 c2 of
>               CatalogDiff [] [] -> liftIO $ return ()
>               c -> liftIO $ assertFailure $ s ++ ", catalogs different: " ++ ppCatDiff c
>     -- adjust tree is the normalization that we run on the original ast as
>     -- well as the dumped ast
>     adjTree :: [Statement] -> [Statement]
>     adjTree = canonicalizeTypeNames . resetAnnotations
>     failIfTypeErrors xast = do
>       let te :: [TypeError]
>           te = [x | x <- universeBi xast]
>       when (not $ null te) $ throwError $ show te

take the parse tree and change the type names to the canonical versions

> canonicalizeTypeNames :: Data a => a -> a
> canonicalizeTypeNames =
>   transformBi $ \x ->
>       case x of
>         SimpleTypeName a tn -> SimpleTypeName a $ canonicalizeTypeName tn
>         x1 -> x1

--------------------------------------------------------------------------------

~~~~

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

~~~~

> adjustAstToLookLikeDump :: [Statement] -> [Statement]
> adjustAstToLookLikeDump ast =
>   (addPresets . stripDml . addConstraintNames) ast
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
>     addPresets = adjustForCreatePlpgsql . (presets ++)
>     adjustForCreatePlpgsql =
>       transformBi $ \x ->
>           case x of
>             s@(Set _ "search_path" _):s1@(CreateLanguage _ _):s2 -> s1:s:s2
>             z -> z
>     presets = [Set ea "statement_timeout" [SetNum ea 0.0]
>               ,Set ea "client_encoding" [SetStr ea "UTF8"]
>               ,Set ea "standard_conforming_strings" [SetId ea "off"]
>               ,Set ea "check_function_bodies" [SetId ea "false"]
>               ,Set ea "client_min_messages" [SetId ea "warning"]
>               ,Set ea "escape_string_warning" [SetId ea "off"]]
>               -- if there are no statements, pg_dump doesn't spit out the search path
>               ++ if null noDml then [] else
>                      [Set ea "search_path" [SetId ea "public", SetId ea "pg_catalog"]]
>               -- these two sets get added if there are create tables
>               ++ case flip find ast (\s ->
>                                   case s of
>                                     CreateTable _ _ _ _ -> True
>                                     _ -> False) of
>                    Nothing -> []
>                    Just _ -> [Set ea "default_tablespace" [SetStr ea ""]
>                              ,Set ea "default_with_oids" [SetId ea "false"]]
>     -- dml statements don't appear in the dump
>     stripDml = filter (\s -> case s of
>                                SelectStatement _ _ -> False
>                                Insert _ _ _ _ _ -> False
>                                Update _ _ _ _ _ _ -> False
>                                Delete _ _ _ _ _ -> False
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

> wrapETT :: (Show e) => ErrorT e IO () -> IO ()
> wrapETT c = runErrorT c >>= \x ->
>          case x of
>            Left er -> assertFailure $ show er
>            Right l -> return l

> ea :: Annotation
> ea = emptyAnnotation
