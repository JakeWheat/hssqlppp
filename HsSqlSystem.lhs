#! /usr/bin/env runghc

Copyright 2009 Jake Wheat

Command line access to a bunch of utility functions.

run
./HsSqlSystem.lhs -?
to get a list of commands and purpose and usage info

> {-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

> import System.Console.CmdArgs
> import System.IO
> import System.Directory
> import Data.List
> import Control.Monad.Error
> import Data.Char
> --import Control.Applicative

> import Test.Framework (defaultMainWithArgs)

> import Database.HsSqlPpp.Tests.ParserTests
> import Database.HsSqlPpp.Tests.TypeCheckTests
> import Database.HsSqlPpp.Tests.ExtensionTests
> import Database.HsSqlPpp.Tests.ParameterizedStatementTests

> import Database.HsSqlPpp.Ast.Environment
> import Database.HsSqlPpp.Ast.Ast
> import Database.HsSqlPpp.Commands.CommandComponents as C

> data HsSqlSystem = Lex {files :: [String]}
>                  | Parse {files :: [String]}
>                  | ParseExpression {files :: [String]}
>                  | Pppp {files :: [String]}
>                  | Ppp {files :: [String]}
>                  | TypeCheck {database :: String
>                              ,files :: [String]}
>                  | TypeCheckExpression {database :: String
>                                        ,files :: [String]}
>                  | AllAnnotations {database :: String
>                                   ,files :: [String]}
>                  | AnnotateSource {database :: String
>                                   ,file :: String}
>                  | PPCatalog {database :: String
>                              ,files :: [String]}
>                  | Clear {database :: String}
>                  | Load {database :: String
>                         ,files :: [String]}
>                  | ClearLoad {database :: String
>                              ,files :: [String]}
>                  | DBCatalog {database :: String}
>                  | LoadPsql {database :: String
>                             ,files :: [String]}
>                  | PgDump {database :: String}
>                  | TestBattery {database :: String
>                                ,files :: [String]}
>                  | Test {extra :: [String]}
>                  | GenWrap {database :: String
>                            ,file :: String}
>                  | BuildDocs
>                    deriving (Show, Data, Typeable)

> main :: IO ()
> main = do
>        cmd <- cmdArgs "HsSqlSystem, Copyright Jake Wheat 2009"
>                       [lexA, parseA, ppppA, pppA,
>                        parseExpressionA, typeCheckExpressionA,
>                        typeCheckA,allAnnotationsA,
>                        annotateSourceA, ppCatalogA,
>                        clearA, loadA, clearLoadA, catalogA, loadPsqlA,
>                        pgDumpA, testBatteryA,
>                        testA,buildDocsA, genWrapA]

>        case cmd of
>          Lex fns -> lexFiles fns
>          Parse fns -> showAst fns
>          Pppp fns -> testPppp fns
>          Ppp fns -> showAst fns
>          ParseExpression fns -> parseExpression fns
>          TypeCheck db fns -> typeCheck2 db fns
>          TypeCheckExpression db fns -> typeCheckExpression db fns
>          AllAnnotations db fns -> typeCheck2 db fns
>          AnnotateSource db fn -> annotateSourceF db fn
>          PPCatalog db fns -> ppCatalog db fns
>          Clear db -> cleardb db
>          Load db fns -> loadSql db fns
>          ClearLoad db fns -> clearAndLoadSql db fns
>          DBCatalog db -> readCat db
>          LoadPsql db fns -> loadSqlPsql db fns
>          PgDump db -> pgDump1 db
>          TestBattery db fns -> runTestBattery db fns
>          Test as -> runTests as
>          BuildDocs -> buildDocs
>          GenWrap db f -> genWrap db f

would like to have the database argument be a common arg, don't know
how to do this

> lexA, parseA, ppppA, pppA, annotateSourceA, clearA, loadA,
>   clearLoadA, catalogA, loadPsqlA, pgDumpA, testBatteryA,
>   typeCheckA, testA, parseExpressionA, typeCheckExpressionA,
>   buildDocsA, allAnnotationsA, ppCatalogA, genWrapA :: Mode HsSqlSystem

===============================================================================

> lexA = mode Lex {files = def &= typ "FILES" & args}
>        &= text "lex the files given and output the tokens on separate lines"

> lexFiles :: [FilePath] -> IO ()
> lexFiles fns = wrapET $
>                forM_ fns (\f ->
>                     message ("lexing " ++ f) >>
>                     readInput f >>= lexSql f >>= printList)

================================================================================

> parseA = mode $ Parse {files = def &= typ "FILES" & args}
>          &= text "Parse files and output the asts"

> showAst :: [String] -> IO ()
> showAst = wrapET . mapM_ (\f ->
>                message ("-- ast of " ++ f) >>
>                readInput f >>= parseSql1 f >>= stripAnn >>= ppSh >>= message)

================================================================================

> ppppA = mode $ Pppp {files = def &= typ "FILES" & args}
>         &= text "Routine to parse sql from a file, pretty print it then parse it \
>                 \again and check the post pretty printing ast is the same as the \
>                 \initial ast"

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

> pppA = mode $ Ppp {files = def &= typ "FILES" & args}
>        &= text "Parse then pretty print some sql so you can check the result \
>               \hasn't mangled the sql."

> ppp :: String -> IO()
> ppp f = wrapET $ message ("--ppp " ++ f) >>
>         readInput f >>= parseSql1 f >>= ppSql >>= message

================================================================================

> parseExpressionA = mode $ ParseExpression {files = def &= typ "FILES" & args}
>                    &= text "Parse files each containing one expression \
>                            \and output the asts"

> parseExpression :: [String] -> IO ()
> parseExpression = wrapET . mapM_ (\f ->
>                message ("-- ast of " ++ f) >>
>                readInput f >>= parseExpression1 f >>= stripAnn
>                >>= ppSh >>= message)

================================================================================

> annotateSourceA = mode $ AnnotateSource {database = def
>                                         ,file = def &= typ "FILE"}
>                   &= text "reads a file, parses, type checks, then outputs info on \
>                           \each statement interspersed with the original source code"

> annotateSourceF :: String -> FilePath -> IO ()
> annotateSourceF db f =
>   wrapET $ do
>     message ("--annotated source of " ++ f)
>     src <- readInput f
>     cat <- readCatalog db
>     parseSql1 f src >>= typeCheckC cat >>= lsnd >>=
>       ppAnnOrig False src >>= message

================================================================================

> ppCatalogA = mode $ PPCatalog {database = def
>                               ,files = def &= typ "FILES" & args}
>              &= text "reads each file, parses, type checks, then outputs the \
>                      \changes to the catalog that the sql makes"

> ppCatalog :: String -> [FilePath] -> IO ()
> ppCatalog db fns = wrapET $ do
>   scat <- readCatalog db
>   (ncat, _) <- mapM (\f -> readInput f >>=
>                            parseSql1 f) fns >>=
>                  lconcat >>= typeCheckC scat
>   compareCatalogs scat emptyEnvironment ncat >>=
>       ppCatDiff >>= message

================================================================================

> typeCheckA = mode $ TypeCheck {database = def
>                               ,files = def &= typ "FILES" & args}
>              &= text "reads each file, parses, type checks, then outputs any type errors"


> typeCheck2 :: String -> [FilePath] -> IO ()
> typeCheck2 db fns = wrapET $
>   readCatalog db >>= \cat ->
>   mapM (\f -> readInput f >>= parseSql1 f) fns >>= lconcat >>=
>   typeCheckC cat >>= lsnd >>= getTEs >>= ppTypeErrors >>= putStrLnList

================================================================================

> allAnnotationsA = mode $ AllAnnotations {database = def
>                                        ,files = def &= typ "FILES" & args}
>                   &= text "reads each file, parses, type checks, then pretty prints the \
>                           \ast with all annotations except the source positions"

> allAnnotations :: String -> [FilePath] -> IO ()
> allAnnotations db fns = wrapET $
>   readCatalog db >>= \cat ->
>   mapM (\f -> readInput f >>= parseSql1 f) fns >>= lconcat >>=
>   typeCheckC cat >>= lsnd >>= ppSh >>= message


================================================================================

> typeCheckExpressionA = mode $ TypeCheckExpression {database = def
>                               ,files = def &= typ "FILES" & args}
>      &= text "reads each file, parses as expression, \
>              \ type checks, then outputs the type or any type errors"


> typeCheckExpression :: String -> [FilePath] -> IO ()
> typeCheckExpression db fns = wrapET $ do
>   aasts <- readCatalog db >>= \cat ->
>               forM fns (\f -> readInput f >>= parseExpression1 f
>                                    >>= typeCheckExpressionC cat)
>   tes <- mapM getTEs aasts
>   mapM_ (\x -> ppTypeErrors x >>= putStrLnList) $ filter (not . null) tes
>   mapM_ (\a -> liftM (show . head) (getTopLevelTypes a) >>= message) aasts

================================================================================

= small hack utility to help with testing

TODO: use the correct username in this command
TODO: do something more correct

> clearA = mode $ Clear {database = def}
>          &= text "hacky util to clear a database"

> cleardb :: String -> IO ()
> cleardb = wrapET . clearDB

================================================================================

= load sql file

> loadA = mode $ Load {database = def
>                     ,files = def &= typ "FILES" & args}
>         &= text "This takes one or more files with sql source text, \
>            \parses them then loads them into the database given."

> loadSql :: String -> [String] -> IO ()
> loadSql db fns = wrapET $
>      liftIO (hSetBuffering stdout NoBuffering) >>
>      mapM (\f -> readInput f >>= parseSql1 f) fns >>= lconcat >>=
>      runExtensions >>= loadAst db ""

================================================================================

> loadPsqlA = mode $ LoadPsql {database = def
>                             ,files = def &= typ "FILES" & args}
>             &= text "loads sql into a database using psql."

> loadSqlPsql :: String -> [String] -> IO ()
> loadSqlPsql db = wrapET .
>   --srcs <- mapM readInput fns
>   --mapM_ (\s -> loadSqlUsingPsql db s >>= message) (map snd srcs)
>   mapM_ (\s -> loadSqlUsingPsqlFromFile db s >>= message)

================================================================================

> pgDumpA = mode $ PgDump {database = def}
>           &= text "run pg dump, used for testing."

> pgDump1 :: String -> IO ()
> pgDump1 db = wrapET $ pgDump db >>= message


================================================================================

might try to work out a way of running multiple commands in one invoc
of this exe, then this command will disappear

> clearLoadA = mode $ ClearLoad {database = def
>                               ,files = def &= typ "FILES" & args}
>              &= text "cleardb then loadsql"

> clearAndLoadSql :: String -> [String] -> IO ()
> clearAndLoadSql db fns = cleardb db >> loadSql db fns

================================================================================

This reads an catalog from a database and writes it out using show.

> catalogA = mode $ DBCatalog {database = def}
>            &= text "read the catalog for the given db and dumps it in source \
>                    \format, used to create the catalog value for template1"

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
(anyast -> rearrange and reorder) == (anyast -> pg->pgdump)

> testBatteryA = mode $ TestBattery {database = def
>                                   ,files = def &= typ "FILES" & args}
>                &= text "runs a load of consistency tests on the sql passed"

> runTestBattery :: String -> [FilePath] -> IO ()
> runTestBattery dbName fns = wrapET $ do
>     clearDB dbName
>     startingCat <- readCatalog dbName
>     (originalCat :: Environment,
>      originalAast :: StatementList) <-
>        mapM (\f -> readInput f >>= parseSql1 f) fns >>= lconcat >>=
>        runExtensions >>= typeCheckC startingCat

>     headerMessage "type errors from initial parse:\n"
>     getTEs originalAast >>= ppTypeErrors >>= putStrLnList

>     mapM_ (\s -> loadSqlUsingPsqlFromFile dbName s >>= message) fns
>     properCat <- readCatalog dbName
>     headerMessage "catalog differences from initial parse and vanilla load:\n"
>     compareCatalogs startingCat originalCat properCat >>=
>       ppCatDiff >>= message

>     (dumpCat,dumpAast) <-
>       pgDump dbName >>= parseSql1 "dump" >>= typeCheckC startingCat

>     headerMessage "type errors from dump:\n"
>     getTEs dumpAast >>= ppTypeErrors >>= putStrLnList

>     headerMessage "catalog differences from initial parse and rechecked pg dump:\n"
>     compareCatalogs startingCat originalCat dumpCat >>=
>       ppCatDiff >>= message

>     message "complete!"
>     where
>       headerMessage = message . ("-----------------------------\n" ++)

================================================================================

> testA = mode $ Test {extra = def &= typ "ANY" & args & unknownFlags}
>         &= text "run automated tests, uses test.framework can pass arguments \
>                 \to this e.g. HsSqlSystem test -t parserTests"

> runTests :: [String] -> IO ()
> runTests as =
>   flip defaultMainWithArgs as $
>     parserTests ++
>     typeCheckTests ++
>     --parameterizedStatementTests ++
>     [extensionTests]

================================================================================

options: nothing -> create/overwrite folders and files
         clean -> delete generated files

create target folder if doesn't exist

> buildDocsA = mode $ BuildDocs
>         &= text "build the documention in the docs/ folder"

> buildDocs :: IO ()
> buildDocs = wrapET $ do
>              liftIO $ createDirectoryIfMissing False "docs/build"
>              inputFiles <- sources
>              let sts = zip (map ("docs/" ++) inputFiles) $
>                        flip map inputFiles
>                             (\f -> "docs/build/" ++ f ++ ".html")
>              forM_ sts $ \(s,t) ->
>                      readInput s >>= pandoc >>= C.writeFile t
>             where
>               sources = liftM (filter (isSuffixOf ".txt"))
>                           (liftIO (getDirectoryContents "docs"))

================================================================================

> genWrapA = mode $ GenWrap {database = def
>                           ,file = def &= typ "FILE"}
>            &= text "experimental code to generate typesafe haskell wrapper \
>                    \for db access"
> genWrap :: String -> String -> IO ()
> genWrap db f =
>   wrapET doit
>     where
>       doit :: (MonadIO m) => ErrorT AllErrors m ()
>       doit = wrapperGen1 db f >>= message

================================================================================

TODOS

command to display all annotation inline in sql source
better catalog diff, e.g. if two tables differ in one column just show
that difference
see if can improve ppshow to use slightly less newlines
command to show which bits of preexisting catalog are used
command to list which views/functions/etc. aren't used anywhere
replace a bunch of commands with composible set:
parse(expression), ppp, typecheck(expression), allannots
->
parse (expression option)
type check
annotation filter + options
output: ast, sql, annotations + options




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

graphviz stuff: dependency graph: pass a view name or function and it
draws a graph of all the views, tables and functions which are used by
that view or function

run an extension by name over some sql source to view differences: add
integration with external diff viewers, so can see before and after,
maybe option to either view pp'd sql, annotated pp'd sql, ast, aast,
etc.  - can also use this for pppsql and pppexpression to view how the
pretty printer mangles things, and for testing, etc.

logging/verbosity:

want a way to log to stderr/stdout/ files with different verbosity
settings

command line arguments:
options:
--database=xxx
?
--extensions
--username
--password
?other connection info
annotation control?

command name
other command arguments:
- on its own is read from stdin
--input='xxx' add literal input rather than from file/stdin

