
This file is a confused mess. The plan is to completely start again,
to produce a big multi function routine which analyzes sql:

* check it parses
* lists type errors
* produces documentation
* catalog reference docs - so we can search an index, then click to go
  to definition in the rendered docs
* check the roundtripping, etc.

This will be mostly based on the runtestbattery function below.

> {-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables,FlexibleContexts,QuasiQuotes #-}
> module Database.HsSqlPpp.Utils.RoundTripTester
>     (roundTripTest
>     ,RoundTripResults(..)
>     ,TypeErrorList
>     ,rtShowBrief) where
>
> import System.Console.CmdArgs
> --import System.IO
> import Control.Monad.Error
> import Data.List

> --import Debug.Trace
> import Data.Maybe
> import Data.Generics.Uniplate.Data
>
> --import Database.HsSqlPpp.Tests.Tests
> import Database.HsSqlPpp.Utils.Utils
> --import Database.HsSqlPpp.Utils.Here
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.SqlTypes
>
> import Database.HsSqlPpp.Parser
> --import Database.HsSqlPpp.Parsing.Lexer
>
> import Database.HsSqlPpp.PrettyPrinter
>
> --import Database.HsSqlPpp.Examples.AnnotateSource
>
> --import Database.HsSqlPpp.DatabaseLoader.DatabaseLoader
> --import Database.HsSqlPpp.Examples.WrapperGen
> import Database.HsSqlPpp.Utils.DBUtils
>
> --import Database.HsSqlPpp.DevelTools.MakeWebsite
> --import Database.HsSqlPpp.DevelTools.MakeAntiNodes
> --import Database.HsSqlPpp.Examples.Extensions.TransitionConstraints
> --import Database.HsSqlPpp.Examples.Extensions.ChaosExtensions
> --import Database.HsSqlPpp.Examples.Chaos2010

> type TypeErrorList = [(Maybe (String,Int,Int), [TypeError])]

> data RoundTripResults = RoundTripResults
>     {rtDatabaseName :: String
>     ,rtFiles :: [FilePath]
>     ,rtEmptyCat :: Catalog
>     ,rtOrigCat :: Catalog
>     ,rtOrigAst :: [Statement]
>     ,rtOrigTypeErrors :: TypeErrorList
>     ,rtPgCat :: Catalog
>     ,rtOrigPgCatDiff :: CatalogDiff
>     ,rtDumpAst :: [Statement]
>     ,rtDumpCat :: Catalog
>     ,rtDumpTypeErrors :: TypeErrorList
>     ,rtOrigDumpCatDiff :: CatalogDiff
>     }

> rtShowBrief :: RoundTripResults -> String
> rtShowBrief rtt =
>   header "initial type errors"
>   ++ intercalate "\n" (ppTypeErrors (rtOrigTypeErrors rtt))
>   ++ header "cat diff: orig to pg"
>   ++ ppCatDiff (rtOrigPgCatDiff rtt)
>   ++ header "dump type errors"
>   ++ intercalate "\n" (ppTypeErrors (rtDumpTypeErrors rtt))
>   ++ header "cat diff: orig to dump"
>   ++ ppCatDiff (rtOrigDumpCatDiff rtt)
>   where
>     header x = "-------------" ++ x ++ "\n"

> {-ppCatalogA = mode $ PPCatalog {database = def
>                               ,files = def &= typ "FILES" & args}
>              &= text "reads each file, parses, type checks, then outputs the \
>                      \changes to the catalog that the sql makes"
>
> ppCatalog :: String -> [FilePath] -> IO ()
> ppCatalog db fns = wrapETs $ do
>   scat <- liftIO (readCatalog db) >>= tsl
>   (ncat, _) <- mapM (\f -> (liftIO . readInput) f >>=
>                            tsl . P.parseSql f) fns >>=
>                  return . (concat |>
>                            astTransformer |>
>                            A.typeCheck scat)
>   liftIO $ putStrLn $ ppCatDiff $ compareCatalogs scat emptyCatalog ncat

-------------------------------------------------------------------------------

load
====

load sql files into a database via parsing and pretty printing them

>
> loadSql :: String -> [String] -> IO ()
> loadSql db fns = wrapETs $
>      liftIO (hSetBuffering stdout NoBuffering) >>
>      mapM (\f -> (liftIO . readInput) f >>=
>                  tsl . P.parseSql f) fns >>=
>      return . (concat |>
>                astTransformer) >>=
>      liftIO . loadAst db

-------------------------------------------------------------------------------

loadPsql
========

load sql files into a database via psql

> loadPsqlA = mode $ LoadPsql {database = def
>                             ,files = def &= typ "FILES" & args}
>             &= text "loads sql into a database using psql."
>
> loadSqlPsql :: String -> [String] -> IO ()
> loadSqlPsql db = wrapETs .
>   mapM_ (\s -> liftIO (loadSqlUsingPsqlFromFile db s) >>=
>                tsl >>=
>                liftIO . putStrLn)


-------------------------------------------------------------------------------

clearLoad
=========

like load above, but runs the clear command first

might try to work out a way of running multiple commands in one invoc
of this exe, then this command will disappear

> clearLoadA = mode $ ClearLoad {database = def
>                               ,files = def &= typ "FILES" & args}
>              &= text "cleardb then loadsql"
>
> clearAndLoadSql :: String -> [String] -> IO ()
> clearAndLoadSql db fns = cleardb db >> loadSql db fns -}


-------------------------------------------------------------------------------

testBattery
===========

run test battery: run a bunch of tests including consistency on the
database and sql files given

The idea is to typecheck the sql, load it into pg and dump it via psql
and via database loader, can then compare asts, catalogs, etc. in a
lot of different ways

currently:
parse and type check sql, save the catalog
load the sql into the db using psql, compare the catalog read from pg
with the catalog from typechecking
dump the sql and typecheck the dump, compare the catalog from this
check with the catalog from the original typecheck

todo: compare asts from initial parse with parsed dump - this is going
to be a lot of work to get passing since the statements are
re-ordered, and sometimes changed/ split up by pg

also: load the sql using the extension system and database loader,
then compare pg catalog with initial catalog, and dump and compare ast
with original ast

want to run a similar set of tests starting with the dump sql:
get ast,cat from dump sql, load using psql and using databaseloader
and check cats and subsequent dump asts.

getting the dump ast comparing with the original ast:

step one: convert tests in parser test to also roundtrip through
database, see parsertests for details

step two: write an ast conversion routine: assume that the pgdump ast
is like the ast fed into pg but with a few statements split into
components (e.g. create table with serial is split into create
sequence and create table), and then the statements are reordered, so
write a routine to mirror this - will then have
(anyast -> rearrange and reorder) == (anyast -> pg->pgdump)


rough new plan:

combine this with the planned report generator

have new annotation routine used in website, in annotatesource2
first part is to run this and produce html report of the source,
then add the catalog summary page, and list of type errors as with the website generator

then we do the round trip tests:
load into database, then compare catalogs
dump from database, reparse and compare catalogs
then parse, typecheck the dumped code, list type errors and catalog differences
when the code is up to it, compare the original ast to the dumped ast.

make different stages optional:

run as a check tool, just want the catalog differences and type errors
on the command line. would be nice to try and link the catalog
differences to source positions.

do website generation, without the pg roundtrips

> parseFiles :: [String] -> IO [StatementList]
> parseFiles fns = do
>   as <- mapM (\f -> fmap (parseSql f) $ readFile f) fns
>   return $ either (error . show) id $ sequence as

> roundTripTest :: ([Statement] -> [Statement]) -> String -> [FilePath] -> IO RoundTripResults
> roundTripTest astTransformer dbName fns = wrapETs $ do
>     -- clear target database
>     liftIO $ clearDBN dbName
>     -- get the catalog of the empty database
>     emptyCat <- readCat dbName
>     -- get the ast and catalog of the sql to test with the catalog
>     -- determined by the hssqlppp typechecker
>     (origAst :: [Statement]) <- (astTransformer . concat) `fmap` liftIO (parseFiles fns)
>     let (origCat :: Catalog, origAast :: [Statement]) = typeCheck emptyCat origAst
>     let origTypeErrors = getTypeErrors origAast
>     -- load the test sql into postgresql using psql and get the
>     -- new catalog from postgresql
>     _ <- liftIO $ loadSqlUsingPsql dbName $ printSql origAst
>     pgCat <- readCat dbName
>     -- show the differences between the catalog as determined by the
>     -- hssqlppp type checker and by loading into postgresql and reading
>     -- the catalog from the loaded database
>     let origPgCatDiff = compareCatalogs emptyCat origCat pgCat
>     -- dump the database from postgresql, parse and run the dump sql through the
>     -- hssqlppp type checker
>     dumpSql <- liftIO $ pgDump dbName
>     dumpSqlAst <- etsr $ parseSql "" dumpSql
>     let (dumpCat,dumpAast) = typeCheck emptyCat dumpSqlAst
>     let dumpTypeErrors = getTypeErrors dumpAast
>     let origDumpCatDiff = compareCatalogs emptyCat origCat dumpCat
>
>     return $ RoundTripResults
>                {rtDatabaseName = dbName
>                ,rtFiles = fns
>                ,rtEmptyCat = emptyCat
>                ,rtOrigCat = origCat
>                ,rtOrigAst = origAast
>                ,rtOrigTypeErrors = origTypeErrors
>                ,rtPgCat = pgCat
>                ,rtOrigPgCatDiff = origPgCatDiff
>                ,rtDumpAst = dumpAast
>                ,rtDumpCat = dumpCat
>                ,rtDumpTypeErrors = dumpTypeErrors
>                ,rtOrigDumpCatDiff = origDumpCatDiff
>                }
>     where
>       readCat d = liftIO (readCatalog d) >>= etsr

> -- | Pretty print list of type errors with optional source position
> --   in emacs readable format.
> ppTypeErrors :: [(Maybe (String,Int,Int), [TypeError])] -> [String]
> ppTypeErrors tes =
>   map showSpTe tes
>   where
>     showSpTe (Just (fn,l,c), e) =
>         fn ++ ":" ++ show l ++ ":" ++ show c ++ ":\n" ++ show e
>     showSpTe (_,e) = "unknown:0:0:\n" ++ show e

> getTypeErrors :: Data a => a -> [(Maybe (String,Int,Int), [TypeError])]
> getTypeErrors es =
>   let as = [(a::Annotation) | a <- universeBi es]
>   in mapMaybe getTes as
>   where
>     getTes as = let tes = errs as
>                 in if null tes
>                    then Nothing
>                    else Just (asrc as, tes)

> type ErrorIO e a = ErrorT e IO a

> etsr :: Show e => Either e a -> ErrorIO String a
> etsr = either (throwError . show) return
