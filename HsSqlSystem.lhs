#! /usr/bin/env runhaskell

Copyright 2009 Jake Wheat

Command line access to a bunch of utility functions.

command line is
./HsSqlSystem.lhs [commandName] [commandArgs ...]

run
./HsSqlSystem.lhs help
to get a list of commands and purpose and usage info

> {-# LANGUAGE ScopedTypeVariables #-}

> import System
> import System.IO
> import Data.List
> import Data.Either
> import Control.Applicative
> import Control.Monad.Error
> import Data.Char

> import Test.Framework (defaultMainWithArgs)

> import Database.HsSqlPpp.Tests.ParserTests
> import Database.HsSqlPpp.Tests.AstCheckTests
> import Database.HsSqlPpp.Tests.ExtensionTests

> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Annotator
> import Database.HsSqlPpp.Ast.Environment
> import Database.HsSqlPpp.Ast.Ast
> import Database.HsSqlPpp.Commands.Commands

================================================================================

List of all the available commands

> commands :: [CallEntry]
> commands = [helpCommand
>            -- parsing
>            ,lexFileCommand
>            ,showAstCommand
>            ,testPpppCommand
>            ,pppCommand
>            -- typechecking
>            ,annotateSourceCommand
>            ,typeCheckCommand
>            -- dbms interaction
>            ,clearDBCommand
>            ,loadSqlCommand
>            ,clearAndLoadSqlCommand
>            ,readCatalogCommand
>            -- pg exe wrappers
>            ,loadSqlPsqlCommand
>            ,pgDumpCommand
>            -- run a battery of tests over some sql
>            ,runTestBatteryCommand
>            -- run the automated tests
>            ,testCommand]

================================================================================

> lexFileCommand :: CallEntry
> lexFileCommand = CallEntry
>                  "lexfile"
>                  "lex the file given and output the tokens on separate lines"
>                  (Single lexFile)


> lexFile :: FilePath -> IO ()
> lexFile f = wrapET $ message ("lexing " ++ f) >>
>             readInput f >>= lexSql f >>= printList

================================================================================

> showAstCommand :: CallEntry
> showAstCommand = CallEntry
>                    "showast"
>                    "Parse files and output the asts"
>                    (Multiple showAst)

> showAst :: [String] -> IO ()
> showAst = wrapET . mapM_ (\f ->
>                message ("-- ast of " ++ f) >>
>                readInput f >>= parseSql1 f >>= stripAnn >>= ppSh >>= message)

================================================================================

> testPpppCommand :: CallEntry
> testPpppCommand =
>   CallEntry "testpppp"
>     "Routine to parse sql from a file, pretty print it then parse it \
>     \again and check the post pretty printing ast is the same as the \
>     \initial ast"
>     (Multiple testPppp)

> testPppp :: [String] -> IO ()
> testPppp = wrapET . mapM_ (\f -> do
>             ast1 <- readInput f >>= parseSql1 f >>= stripAnn
>             ast2 <- ppSql ast1 >>= parseSql1 "" >>= stripAnn
>             if ast1 /= ast2
>                then do
>                     message "asts are different\n-- original"
>                     ppSh ast1 >>= message
>                     message "-- ppp'd"
>                     ppSh ast2 >>= message
>                else message "success")

================================================================================

> pppCommand :: CallEntry
> pppCommand =
>   CallEntry "ppp"
>     "Parse then pretty print some sql so you can check the result \
>     \hasn't mangled the sql."
>     (Single ppp)

> ppp :: String -> IO()
> ppp f = wrapET $ message ("--ppp " ++ f) >>
>         readInput f >>= parseSql1 f >>= ppSql >>= message

================================================================================

> annotateSourceCommand :: CallEntry
> annotateSourceCommand =
>   CallEntry "annotatesource"
>     "reads a file, parses, type checks, then outputs info on \
>     \each statement interspersed with the original source code"
>     (Single annotateSourceF)

> annotateSourceF :: FilePath -> IO ()
> annotateSourceF f =
>   wrapET $ do
>     message ("--annotated source of " ++ f)
>     src <- readInput f
>     parseSql1 f src >>= annotate defaultTemplate1Environment >>= lsnd >>=
>       ppAnnOrig False src >>= message

================================================================================

> typeCheckCommand :: CallEntry
> typeCheckCommand =
>   CallEntry "typecheck"
>     "reads each file, parses, type checks, then outputs any type errors"
>     (Multiple typeCheck)

> typeCheck :: [FilePath] -> IO ()
> typeCheck fns = wrapET $
>   readCatalog (head fns) >>= \cat ->
>   mapM (\f -> readInput f >>= parseSql1 f) (tail fns) >>= lconcat >>=
>   annotate cat >>= lsnd >>= getTEs >>= ppTypeErrors >>= putStrLnList

================================================================================

= small hack utility to help with testing

TODO: use the correct username in this command
TODO: do something more correct

> clearDBCommand :: CallEntry
> clearDBCommand = CallEntry
>                  "cleardb"
>                  "hacky util to clear a database"
>                  (Single cleardb)

> cleardb :: String -> IO ()
> cleardb db = wrapET $ clearDB db

================================================================================

= load sql file

> loadSqlCommand :: CallEntry
> loadSqlCommand = CallEntry
>                  "loadsql"
>                  "This takes one or more files with sql source text, \
>                  \parses them then loads them into the database given."
>                  (Multiple loadSql)

> loadSql :: [String] -> IO ()
> loadSql args = wrapET $
>   let (db:fns) = args
>   in liftIO (hSetBuffering stdout NoBuffering) >>
>      mapM (\f -> readInput f >>= parseSql1 f) fns >>= lconcat >>=
>      runExtensions >>= loadAst db ""

================================================================================

> loadSqlPsqlCommand :: CallEntry
> loadSqlPsqlCommand = CallEntry
>                  "loadsqlusingpsql"
>                  "loads sql into a database using psql."
>                  (Multiple loadSqlPsql)

> loadSqlPsql :: [String] -> IO ()
> loadSqlPsql args = wrapET $
>   let (db:fns) = args
>   --srcs <- mapM readInput fns
>   --mapM_ (\s -> loadSqlUsingPsql db s >>= message) (map snd srcs)
>   in mapM_ (\s -> loadSqlUsingPsqlFromFile db s >>= message) fns

================================================================================

> pgDumpCommand :: CallEntry
> pgDumpCommand = CallEntry
>                  "pgdump"
>                  "run pg dump, used for testing."
>                  (Single pgDump1)

> pgDump1 :: String -> IO ()
> pgDump1 db = wrapET $ pgDump db >>= message


================================================================================

might try to work out a way of running multiple commands in one invoc
of this exe, then this command will disappear

> clearAndLoadSqlCommand :: CallEntry
> clearAndLoadSqlCommand = CallEntry
>                          "clearandloadsql"
>                          "cleardb then loadsql"
>                          (Multiple
>                           (\args -> do
>                              cleardb $ head args
>                              loadSql args))

================================================================================

This reads an catalog from a database and writes it out using show.

> readCatalogCommand :: CallEntry
> readCatalogCommand = CallEntry
>                   "readcatalog"
>                   "read the catalog for the given db and dumps it in source \
>                   \format, used to create the catalog value for template1"
>                   (Single readCat)
> readCat :: String -> IO ()
> readCat dbName = wrapET $ do
>   cat <- readCatalog dbName
>   message preamble
>   ppSh cat >>= prefixLines >>= message
>   where
>     preamble = "\n\
>                \Copyright 2009 Jake Wheat\n\
>                \\n\
>                \This file contains\n\
>                \\n\
>                \> {-# OPTIONS_HADDOCK hide  #-}\n\
>                \\n\
>                \> module Database.HsSqlPpp.AstInternals.Environment.DefaultTemplate1Environment\n\
>                \>     (defaultTemplate1Environment\n\
>                \>      ) where\n\
>                \\n\
>                \> import Database.HsSqlPpp.AstInternals.Environment.EnvironmentInternal\n\
>                \> import Database.HsSqlPpp.AstInternals.TypeType\n\
>                \\n\
>                \> defaultTemplate1Environment :: Environment\n\
>                \> defaultTemplate1Environment =\n\
>                \>    (\\l -> case l of\n\
>                \>             Left x -> error $ show x\n\
>                \>             Right e -> e) $\n\
>                \>     updateEnvironment defaultEnvironment\n"
>     prefixLines :: (Monad m, Error e) => String -> ErrorT e m String
>     prefixLines = return . unlines . map (">        " ++) . lines


================================================================================

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
(anyast -> rarrange and reorder) == (anyast -> pg->pgdump)


> runTestBatteryCommand :: CallEntry
> runTestBatteryCommand = CallEntry
>                    "runtestbattery"
>                    "runs a load of consistency tests on the sql passed"
>                    (Multiple runTestBattery)

> runTestBattery :: [FilePath] -> IO ()
> runTestBattery (dbName:fns) = wrapET $ do
>     clearDB dbName
>     startingCat <- readCatalog dbName
>     (originalCat :: Environment ,originalAast :: StatementList) <-
>        mapM (\f -> readInput f >>= parseSql1 f) fns >>= lconcat >>=
>        runExtensions >>= annotate startingCat

>     headerMessage "type errors from initial parse:\n"
>     getTEs originalAast >>= ppTypeErrors >>= putStrLnList

>     mapM_ (\s -> loadSqlUsingPsqlFromFile dbName s >>= message) fns
>     properCat <- readCatalog dbName
>     headerMessage "catalog differences from initial parse and vanilla load:\n"
>     compareCatalogs startingCat originalCat properCat >>=
>       ppCatDiff >>= message

>     (dumpCat,dumpAast) <-
>       pgDump dbName >>= parseSql1 "dump" >>= annotate startingCat

>     headerMessage "type errors from dump:\n"
>     getTEs dumpAast >>= ppTypeErrors >>= putStrLnList

>     headerMessage "catalog differences from initial parse and rechecked pg dump:\n"
>     compareCatalogs startingCat originalCat dumpCat >>=
>       ppCatDiff >>= message

>     message "complete!"
>     return ()
>     where
>       headerMessage m = message $ "-----------------------------\n" ++ m


> runTestBattery _ = error "checkbig not passed at least 2 args"

================================================================================

> testCommand :: CallEntry
> testCommand = CallEntry "test"
>                "run automated tests, uses test.framework can pass arguments \
>                \to this e.g. HsSqlSystem test -t parserTests"
>                (Multiple runTests)
> runTests :: [String] -> IO ()
> runTests args =
>   flip defaultMainWithArgs args [
>     parserTests
>    ,astCheckTests
>    --,databaseLoaderTests
>    ,extensionTests
>    ]

================================================================================

> helpCommand :: CallEntry
> helpCommand = CallEntry
>                  "help"
>                  "use 'help' to see a list of commands\n\
>                  \use 'help all' to see a list of commands with descriptions\n\
>                  \use 'help [command]' to see the description for that command"
>                   (Multiple help)


> help :: [String] -> IO ()
> help args =
>   case args of
>             ["all"] -> showCommands True
>             [x] -> helpForCommand x
>             _ -> showCommands False
>   where
>     showCommands full = do
>       putStrLn "commands available"
>       mapM_ putStrLn $ flip map commands (\(CallEntry nm desc _)  ->
>                                           if full
>                                             then nm ++ "\n" ++ desc ++ "\n"
>                                             else nm ++ "\n")

> helpForCommand :: String -> IO ()
> helpForCommand c =
>     case lookupCaller commands c of
>       Nothing -> putStrLn "unrecognised command" >> help []
>       Just (CallEntry nm desc _) -> putStrLn $ nm ++ "\n" ++ desc

===============================================================================

= main

> main :: IO ()
> main = do
>   args <- getArgs
>   case () of
>        _ | null args -> putStrLn "no command given" >> help []
>          | otherwise -> case lookupCaller commands (map toLower $ head args) of
>                           Nothing -> putStrLn "unrecognised command" >> help []
>                           Just c -> call c $ tail args

> lookupCaller :: [CallEntry] -> String -> Maybe CallEntry
> lookupCaller ce name = find (\(CallEntry nm _ _) -> name == nm) ce




================================================================================

utility function for testing within emacs, just plonked here randomly,
will probably remove when this exe has the ability to read the source
from stdin

> parseAndTypeCheck :: String
>                   -> String
>                   -> IO StatementList
> parseAndTypeCheck dbName src = do
>    case parseSql "" src of
>      Left e -> error $ show e
>      Right ast -> do
>        e <- updateEnvironment defaultEnvironment <$> readEnvironmentFromDatabase dbName
>        case e of
>          Left er -> error $ show er
>          Right env -> return $ annotateAstEnv env ast

================================================================================

> data CallEntry = CallEntry String String CallType
>                --          name   use

> data CallType = Single (String -> IO ())
>               | Multiple ([String] -> IO ())

> call :: CallEntry -> [String] -> IO ()
> call (CallEntry _ _ ct) args =
>     case ct of
>       Single f | length args /= 1 -> error "please call this command with one argument"
>                | otherwise -> f (head args)
>       Multiple f -> f args

================================================================================

TODOS

think of a better name for this command than hssqlsystem

want better options system:
first set of options get passed around in reader? monad:
  database name, username, password for pg
  useextensions
  annotation control? - when showing values, which annotations to also output?
these have defaults, can change defaults in ~/.config/hssqlsystem or something,
and can be overridden using --dbname=...,etc. command line args (make
these work like psql? then have environment variables also)
second set of options is for input and output:
read from file(s), stdin, or from string literal command line arg, e.g.
HsSqlSystem expressionType --src="3 + 5"
output to file(s) or stdout

review command names and arguments:
find a better naming convention: some commands produce haskell values as text,
some produce non haskell compatible text e.g. lexfile
some run tests and produce a success/fail result, maybe a list of issues

run multiple commands in one invocation?

check errors passed to user are understandable

command line commands to add:

showAast
ppCatalog - read from db and print in human readable rather
            than as haskell value
showexpressionast
showexpressionaast
typecheckexpression
pppexpression
showCatalogUpdates - run over some sql files, outputs the catalog changes
                     made by this sql
ppCatalogUpdates

run an extension by name over some sql source to view differences: add
integration with external diff viewers, so can see before and after,
maybe option to either view pp'd sql, annotated pp'd sql, ast, aast,
etc.  - can also use this for pppsql and pppexpression to view how the
pretty printer mangles things, and for testing, etc.

logging/verbosity:

want a way to log to stderr/stdout/ files with different verbosity
settings
