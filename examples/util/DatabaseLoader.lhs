
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
>
> import System.Console.CmdArgs
> import System.IO
> import Control.Monad.Error

> --import Debug.Trace
> import Data.Maybe
> import Data.Generics.Uniplate.Data
>
> --import Database.HsSqlPpp.Tests.Tests
> --import Database.HsSqlPpp.Utils.Utils
> --import Database.HsSqlPpp.Utils.Here
>
> --import Database.HsSqlPpp.Ast
> --import Database.HsSqlPpp.Catalog
> --import qualified Database.HsSqlPpp.TypeChecker as A
> --import Database.HsSqlPpp.Annotation
> --import Database.HsSqlPpp.SqlTypes
>
> --import qualified Database.HsSqlPpp.Parser as P
> --import Database.HsSqlPpp.Parsing.Lexer
>
> --import Database.HsSqlPpp.PrettyPrinter
>
> --import Database.HsSqlPpp.Examples.AnnotateSource
>
> import Database.HsSqlPpp.DatabaseLoader.DatabaseLoader
> --import Database.HsSqlPpp.Examples.WrapperGen
> --import Database.HsSqlPpp.Examples.DBUtils
>
> --import Database.HsSqlPpp.DevelTools.MakeWebsite
> --import Database.HsSqlPpp.DevelTools.MakeAntiNodes
> --import Database.HsSqlPpp.Examples.Extensions.TransitionConstraints
> --import Database.HsSqlPpp.Examples.Extensions.ChaosExtensions
> --import Database.HsSqlPpp.Examples.Chaos2010


> data DatabaseLoader = PPCatalog {database :: String
>                              ,files :: [String]}
>
>                  | Load {database :: String
>                         ,files :: [String]}
>                  | LoadPsql {database :: String
>                             ,files :: [String]}
>                  | PgDump {database :: String}
>                  | Clear {database :: String}
>                  | ClearLoad {database :: String
>                              ,files :: [String]}
>                  | DBCatalog {database :: String}
>
>                  | TestBattery {database :: String
>                                ,files :: [String]}
>
>
>                    deriving (Show, Data, Typeable)


> ppCatalogA = mode $ PPCatalog {database = def
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

> loadA = mode $ Load {database = def
>                     ,files = def &= typ "FILES" & args}
>         &= text "This takes one or more files with sql source text, \
>            \parses them then loads them into the database given."
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

pgDump
======

run pg dump

> pgDumpA = mode $ PgDump {database = def}
>           &= text "run pg dump, used for testing."
>
> pgDump1 :: String -> IO ()
> pgDump1 db = pgDump db >>= putStrLn

-------------------------------------------------------------------------------

clear
=====

small hack utility to help with testing, clears the database given (as
long as your user name is jake!)

TODO: use the correct username in this command
TODO: do something more correct

> clearA = mode $ Clear {database = def}
>          &= text "hacky util to clear a database"
>
> cleardb :: String -> IO ()
> cleardb = clearDB

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
> clearAndLoadSql db fns = cleardb db >> loadSql db fns

-------------------------------------------------------------------------------

dbCatalog
=========

This reads an catalog from a database and writes it out using
show. Warning: these things are big since they contain all the default
PostgreSQL catalog contents.

> catalogA = mode $ DBCatalog {database = def}
>            &= text "read the catalog for the given db and dumps it in source \
>                    \format, used to create the catalog value for template1"
>
> readCat :: String -> IO ()
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
>     prefixLines = unlines . map (">        " ++) . lines

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


> testBatteryA = mode $ TestBattery {database = def
>                                   ,files = def &= typ "FILES" & args}
>                &= text "runs a load of consistency tests on the sql passed"
>
> runTestBattery :: String -> [FilePath] -> IO ()
> runTestBattery dbName fns = wrapETs $ do
>     liftIO $ hSetBuffering stdout NoBuffering
>     liftIO $ hSetBuffering stderr NoBuffering
>     liftIO $ clearDB dbName
>     startingCat <- liftIO (readCatalog dbName) >>= tsl
>     (originalCat :: Catalog,
>      originalAast :: StatementList) <-
>        mapM (\f -> (liftIO . readInput) f >>= tsl . P.parseSql f) fns >>=
>        return . (concat |>
>                  astTransformer |>
>                  A.typeCheck startingCat)
>
>     headerMessage "type errors from initial parse:\n"
>     (return . getTypeErrors) originalAast >>=
>        return . ppTypeErrors >>=
>        mapM_ (liftIO . putStrLn)
>
>     mapM_ (\s -> liftIO (loadSqlUsingPsqlFromFile dbName s) >>= tsl >>=
>                  liftIO . putStrLn) fns
>     properCat <- liftIO (readCatalog dbName) >>= tsl
>
>     headerMessage "catalog differences from initial parse and vanilla load:\n"
>     liftIO $ putStrLn $ ppCatDiff (compareCatalogs startingCat
>                                                    originalCat
>                                                    properCat)
>
>     headerMessage "dump and check\n"
>     (dumpCat,dumpAast) <-
>       liftIO (pgDump dbName) >>=
>       tsl . P.parseSql "dump" >>=
>       return . A.typeCheck startingCat
>
>     headerMessage "type errors from dump:\n"
>     (return . getTypeErrors) dumpAast >>=
>        return . ppTypeErrors >>=
>        mapM_ (liftIO . putStrLn)
>
>     headerMessage "catalog differences from initial parse and rechecked \
>                   \pg dump:\n"
>     liftIO $ putStrLn $ ppCatDiff $ compareCatalogs startingCat
>                                                     originalCat
>                                                     dumpCat
>
>     (liftIO . putStrLn) "complete!"
>     where
>       headerMessage = liftIO . putStrLn .
>                       ("-----------------------------\n" ++)


> main :: IO ()
> main = do
>        cmd <- cmdArgs "HsSqlSystem"
>                       [lexA, parseA, ppppA, pppA,
>                        parseExpressionA, typeCheckExpressionA,
>                        typeCheckA,allAnnotationsA]
>
>        case cmd of
>          Lex fns -> lexFiles fns
>          Parse fns -> showAst fns
>          Pppp fns -> testPppp fns
>          Ppp fns -> ppp fns
>          ParseExpression fns -> parseExpression fns
>          TypeCheck db fns -> typeCheck2 db fns
>          TypeCheckExpression db fns -> typeCheckExpression db fns
>          AllAnnotations db fns -> allAnnotations db fns
>
> lexA, parseA, ppppA, pppA,
>   typeCheckA, parseExpressionA, typeCheckExpressionA,
>   allAnnotationsA :: Mode HsSqlSystem
