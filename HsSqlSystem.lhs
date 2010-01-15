#! /usr/bin/env runghc

Copyright 2009 Jake Wheat

Command line access to a bunch of utility functions.

run
./HsSqlSystem.lhs -?
to get a list of commands and purpose and usage info

> {-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables,FlexibleContexts #-}

> import System.Console.CmdArgs
> import System.IO
> import System.Directory
> import Data.List
> import Control.Monad.Error
> import Data.Char
> import System
> import Data.Generics

> import Text.Show.Pretty
> import System.Process.Pipe
> --import Text.Pandoc

> import Test.Framework (defaultMainWithArgs)

> import Database.HsSqlPpp.Tests.ParserTests
> import Database.HsSqlPpp.Tests.TypeCheckTests
> import Database.HsSqlPpp.Tests.ExtensionTests
> import Database.HsSqlPpp.Tests.ParameterizedStatementTests
> import Database.HsSqlPpp.Tests.RoundtripTests

> import Database.HsSqlPpp.Ast.Environment
> import Database.HsSqlPpp.Ast.Ast

> import Database.HsSqlPpp.Utils

> import qualified Database.HsSqlPpp.Parsing.Parser as P
> import Database.HsSqlPpp.Parsing.Lexer

> import qualified Database.HsSqlPpp.Ast.TypeChecker as A
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Ast.SqlTypes

> import Database.HsSqlPpp.PrettyPrinter.PrettyPrinter
> import Database.HsSqlPpp.PrettyPrinter.AnnotateSource

> import Database.HsSqlPpp.Dbms.DBAccess
> import Database.HsSqlPpp.Dbms.DatabaseLoader

> import Database.HsSqlPpp.Extensions.ChaosExtensions

> --import Database.HsSqlPpp.Utils
> import Database.HsSqlPpp.HsText.HsText

> import Database.HsSqlPpp.Dbms.WrapperGen


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
>                     (liftIO . putStrLn) ("lexing " ++ f) >>
>                     readInput f >>= tsl . lexSqlText f >>= mapM_ (liftIO . print))

================================================================================

> parseA = mode $ Parse {files = def &= typ "FILES" & args}
>          &= text "Parse files and output the asts"

> showAst :: [String] -> IO ()
> showAst = wrapET . mapM_ (\f ->
>                (liftIO . putStrLn) ("-- ast of " ++ f) >>
>                readInput f >>= tsl . P.parseSql f >>=
>                return . stripAnnotations >>= return . ppShow >>= liftIO . putStrLn)

================================================================================

> ppppA = mode $ Pppp {files = def &= typ "FILES" & args}
>         &= text "Routine to parse sql from a file, pretty print it then parse it \
>                 \again and check the post pretty printing ast is the same as the \
>                 \initial ast"

> testPppp :: [String] -> IO ()
> testPppp = wrapET . mapM_ (\f -> do
>             ast1 <- readInput f >>= tsl . P.parseSql f >>= return . stripAnnotations
>             ast2 <- (return . printSql) ast1 >>= tsl . P.parseSql "" >>= return . stripAnnotations
>             if ast1 /= ast2
>                then do
>                     (liftIO . putStrLn) "asts are different\n-- original"
>                     (return . ppShow) ast1 >>= liftIO . putStrLn
>                     (liftIO . putStrLn) "-- ppp'd"
>                     (return . ppShow) ast2 >>= liftIO . putStrLn
>                else (liftIO . putStrLn) "success")

================================================================================

> pppA = mode $ Ppp {files = def &= typ "FILES" & args}
>        &= text "Parse then pretty print some sql so you can check the result \
>               \hasn't mangled the sql."

> ppp :: String -> IO()
> ppp f = wrapET $ (liftIO . putStrLn) ("--ppp " ++ f) >>
>         readInput f >>= tsl . P.parseSql f >>= return . printSql >>= liftIO . putStrLn

================================================================================

> parseExpressionA = mode $ ParseExpression {files = def &= typ "FILES" & args}
>                    &= text "Parse files each containing one expression \
>                            \and output the asts"

> parseExpression :: [String] -> IO ()
> parseExpression = wrapET . mapM_ (\f ->
>                (liftIO . putStrLn) ("-- ast of " ++ f) >>
>                readInput f >>= tsl . P.parseExpression f
>                >>= return . stripAnnotations
>                >>= return . ppShow >>= liftIO . putStrLn)

================================================================================

> annotateSourceA = mode $ AnnotateSource {database = def
>                                         ,file = def &= typ "FILE"}
>                   &= text "reads a file, parses, type checks, then outputs info on \
>                           \each statement interspersed with the original source code"

> annotateSourceF :: String -> FilePath -> IO ()
> annotateSourceF db f =
>   wrapET $ do
>     (liftIO . putStrLn) ("--annotated source of " ++ f)
>     src <- readInput f
>     cat <- readCatalog db
>     tsl (P.parseSql f src) >>= return . A.typeCheck cat >>= return . snd >>=
>       ppAnnOrig False src >>= liftIO . putStrLn

================================================================================

> ppCatalogA = mode $ PPCatalog {database = def
>                               ,files = def &= typ "FILES" & args}
>              &= text "reads each file, parses, type checks, then outputs the \
>                      \changes to the catalog that the sql makes"

> ppCatalog :: String -> [FilePath] -> IO ()
> ppCatalog db fns = wrapET $ do
>   scat <- readCatalog db
>   (ncat, _) <- mapM (\f -> readInput f >>=
>                            tsl . P.parseSql f) fns >>=
>                  return . concat >>= return . A.typeCheck scat
>   ppCatDiff (compareCatalogs scat emptyEnvironment ncat) >>= liftIO . putStrLn

================================================================================

> typeCheckA = mode $ TypeCheck {database = def
>                               ,files = def &= typ "FILES" & args}
>              &= text "reads each file, parses, type checks, then outputs any type errors"


> typeCheck2 :: String -> [FilePath] -> IO ()
> typeCheck2 db fns = wrapET $
>   readCatalog db >>= \cat ->
>   mapM (\f -> readInput f >>= tsl . P.parseSql f) fns >>= return . concat >>=
>   return . A.typeCheck cat >>= return . snd >>= return . A.getTypeErrors >>= ppTypeErrors >>= mapM_ (liftIO . putStrLn)

================================================================================

> allAnnotationsA = mode $ AllAnnotations {database = def
>                                        ,files = def &= typ "FILES" & args}
>                   &= text "reads each file, parses, type checks, then pretty prints the \
>                           \ast with all annotations except the source positions"

> allAnnotations :: String -> [FilePath] -> IO ()
> allAnnotations db fns = wrapET $
>   readCatalog db >>= \cat ->
>   mapM (\f -> readInput f >>= tsl . P.parseSql f) fns >>= return . concat >>=
>   return . A.typeCheck cat >>= return . snd >>= return . ppShow >>= liftIO . putStrLn


================================================================================

> typeCheckExpressionA = mode $ TypeCheckExpression {database = def
>                               ,files = def &= typ "FILES" & args}
>      &= text "reads each file, parses as expression, \
>              \ type checks, then outputs the type or any type errors"


> typeCheckExpression :: String -> [FilePath] -> IO ()
> typeCheckExpression db fns = wrapET $ do
>   aasts <- readCatalog db >>= \cat ->
>               forM fns (\f -> readInput f >>= tsl . P.parseExpression f
>                                    >>= return . A.typeCheckExpression cat)
>   tes <- mapM (return . A.getTypeErrors) aasts
>   mapM_ (\x -> ppTypeErrors x >>= mapM_ (liftIO . putStrLn)) $ filter (not . null) tes
>   mapM_ (\a -> liftM (show . head) (return $ A.getTopLevelTypes [a]) >>= liftIO . putStrLn) aasts

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
>      mapM (\f -> readInput f >>= tsl . P.parseSql f) fns >>= return . concat >>=
>      return . extensionize >>= loadAst db ""

================================================================================

> loadPsqlA = mode $ LoadPsql {database = def
>                             ,files = def &= typ "FILES" & args}
>             &= text "loads sql into a database using psql."

> loadSqlPsql :: String -> [String] -> IO ()
> loadSqlPsql db = wrapET .
>   --srcs <- mapM readInput fns
>   --mapM_ (\s -> loadSqlUsingPsql db s >>= message) (map snd srcs)
>   mapM_ (\s -> loadSqlUsingPsqlFromFile db s >>= liftIO . putStrLn)

================================================================================

> pgDumpA = mode $ PgDump {database = def}
>           &= text "run pg dump, used for testing."

> pgDump1 :: String -> IO ()
> pgDump1 db = wrapET $ pgDump db >>= liftIO . putStrLn


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
>   (liftIO . putStrLn) preamble
>   (return . ppShow) cat >>= prefixLines >>= liftIO . putStrLn
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
>        mapM (\f -> readInput f >>= tsl . P.parseSql f) fns >>= return . concat >>=
>        return . extensionize >>= return . A.typeCheck startingCat

>     headerMessage "type errors from initial parse:\n"
>     (return . A.getTypeErrors) originalAast >>= ppTypeErrors >>= mapM_ (liftIO . putStrLn)

>     mapM_ (\s -> loadSqlUsingPsqlFromFile dbName s >>= liftIO . putStrLn) fns
>     properCat <- readCatalog dbName
>     headerMessage "catalog differences from initial parse and vanilla load:\n"
>     ppCatDiff (compareCatalogs startingCat originalCat properCat) >>= liftIO . putStrLn

>     (dumpCat,dumpAast) <-
>       pgDump dbName >>= tsl . P.parseSql "dump" >>= return . A.typeCheck startingCat

>     headerMessage "type errors from dump:\n"
>     (return . A.getTypeErrors) dumpAast >>= ppTypeErrors >>= mapM_ (liftIO . putStrLn)

>     headerMessage "catalog differences from initial parse and rechecked pg dump:\n"
>     ppCatDiff (compareCatalogs startingCat originalCat dumpCat) >>= liftIO . putStrLn

>     (liftIO . putStrLn) "complete!"
>     where
>       headerMessage = liftIO . putStrLn . ("-----------------------------\n" ++)

================================================================================

> testA = mode $ Test {extra = def &= typ "ANY" & args & unknownFlags}
>         &= text "run automated tests, uses test.framework can pass arguments \
>                 \to this e.g. HsSqlSystem test -t parserTests"

> runTests :: [String] -> IO ()
> runTests as =
>   flip defaultMainWithArgs as $
>     parserTests ++
>     typeCheckTests ++
>     parameterizedStatementTests ++
>     roundtripTests ++
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
>                      readInput s >>= pandoc >>= liftIO . writeFile t
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
>       doit :: (MonadIO m) => ErrorT String m ()
>       doit = wrapperGen1 db f >>= liftIO . putStrLn

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


================================================================================

TODO:
get rid of most of the wrappers below, they were written out of
programmer incompetence with errort


> -- | Pretty print list of type errors with optional source position
> --   in emacs readable format.
> ppTypeErrors :: Monad m =>
>                 [(Maybe AnnotationElement, [TypeError])] -> m [String]
> ppTypeErrors tes =
>   return $ map showSpTe tes
>   where
>     showSpTe (Just (SourcePos fn l c), e) =
>         fn ++ ":" ++ show l ++ ":" ++ show c ++ ":\n" ++ show e
>     showSpTe (_,e) = "unknown:0:0:\n" ++ show e

> -- | Get the top level type annotation from the ast passed.
> --getTopLevelTypes :: (Monad m, Error e, Data d) =>
> --          d -> ErrorT e m [Type]
> --getTopLevelTypes = return . A.getTopLevelTypes . (:[])


================================================================================

pretty printing

todo: change the naming convention, so fns which produce haskell
syntax start with show, human readable stuff starts with pp, not sure
where printsql comes in system though

> -- | use ppshow to pretty print a value.
> --ppSh :: (Monad m, Error e, Show a) => a -> ErrorT e m String
> -- ppSh = return . ppShow

> -- | pretty print an ast.
> -- ppSql :: (Monad m, Error e) => StatementList -> ErrorT e m String
> -- ppSql = return . printSql

> -- | take a source text and annotated ast and interpolate annotations into the source
> --   as comments
> ppAnnOrig :: (Monad m, Error e) => Bool -> String -> StatementList -> ErrorT e m String
> ppAnnOrig doErrs src = return . annotateSource doErrs src

================================================================================

dbms utilities

> -- | get the catalog from the database
> readCatalog :: MonadIO m => String -> ErrorT String m Environment
> readCatalog dbName =
>   liftIO (readEnvironmentFromDatabase dbName) >>=
>   tsl . updateEnvironment defaultEnvironment

> -- | run psql to load the sql text into a database.
> loadSqlUsingPsql :: MonadIO m  => String -> String -> ErrorT String m String
> loadSqlUsingPsql dbName =
>   liftIO . pipeString [("psql", [dbName
>                                 ,"-q"
>                                 ,"--set"
>                                 ,"ON_ERROR_STOP=on"
>                                 ,"--file=-"])]

> -- | run psql to load sql from the filename given into a database.
> loadSqlUsingPsqlFromFile :: MonadIO m  => String -> FilePath -> ErrorT String m String
> loadSqlUsingPsqlFromFile dbName fn = do
>   ex <- liftIO $ system ("psql " ++ dbName ++
>                 " -q --set ON_ERROR_STOP=on" ++
>                 " --file=" ++ fn)
>   case ex of
>     ExitFailure e -> throwError $ "psql failed with " ++ show e
>     ExitSuccess -> return ""

> -- | use the hssqlppp code to load the sql into a database directly
> --   (this parses and pretty prints the sql to load it)
> loadAst :: (MonadIO m, Error e) => String -> String -> StatementList -> ErrorT e m ()
> loadAst db fn = liftIO . loadIntoDatabase db fn

> -- | use a dodgy hack to clear the database given
> clearDB :: MonadIO m => String -> ErrorT String m ()
> clearDB db =
>   liftIO $ withConn ("dbname=" ++ db) $ \conn ->
>     runSqlCommand conn "drop owned by jake cascade;"

> -- | dump the given database to sql source using pg_dump
> pgDump :: MonadIO m => String -> ErrorT String m String
> pgDump db = liftIO $ pipeString [("pg_dump", [db
>                                              ,"--schema-only"
>                                              ,"--no-owner"
>                                              ,"--no-privileges"])] ""

================================================================================

-- catalog stuff - just a diff to compare two catalogs

-- > -- | items in first catalog and not second, items in second and not first.
-- > data CatDiff = CatDiff [EnvironmentUpdate] [EnvironmentUpdate]
-- >                deriving Show

-- > -- | find differences between two catalogs
-- > compareCatalogs :: (Monad m, Error e) => Environment -> Environment -> Environment -> ErrorT e m CatDiff
-- > compareCatalogs base start end =
-- >         let baseEnvBits = deconstructEnvironment base
-- >             startEnvBits = deconstructEnvironment start \\ baseEnvBits
-- >             endEnvBits = deconstructEnvironment end \\ baseEnvBits
-- >             missing = sort $ endEnvBits \\ startEnvBits
-- >             extras = sort $ startEnvBits \\ endEnvBits
-- >         in return $ CatDiff missing extras

> -- | print a catdiff in a more human readable way than show.
> ppCatDiff :: (Monad m, Error e) => CatalogDiff -> ErrorT e m String
> ppCatDiff (CatalogDiff missing extr) =
>           return $ "\nmissing:\n"
>                    ++ intercalate "\n" (map ppEnvUpdate missing)
>                    ++ "\nextra:\n"
>                    ++ intercalate "\n" (map ppEnvUpdate extr)

================================================================================

> -- | Documentation command to produce some hssqlppp docs, takes a
> --   pandoc source file and converts to html, can run and insert
> --   commands embedded in the source
> pandoc :: MonadIO m => String -> ErrorT String m String
> pandoc txt = return txt {-
>   liftM (writeHtmlString wopt . readMarkdown defaultParserState)
>     (hsTextize txt)
>   where
>     wopt = defaultWriterOptions {
>                writerStandalone = True
>               ,writerTitlePrefix = "HsSqlPpp documentation"
>               ,writerTableOfContents = True
>               ,writerHeader = "<style>\n\
>                               \pre {\n\
>                               \    border: 1px dotted gray;\n\
>                               \    background-color: #ececec;\n\
>                               \    color: #1111111;\n\
>                               \    padding: 0.5em;\n\
>                               \}\n\
>                               \</style>"
>              }-}


writerStandalone :: Bool	Include header and footer
writerHeader :: String	Header for the document
writerTitlePrefix :: String	Prefix for HTML titles
writerTabStop :: Int	Tabstop for conversion btw spaces and tabs
writerTableOfContents :: Bool	Include table of contents
writerS5 :: Bool	We're writing S5
writerHTMLMathMethod :: HTMLMathMethod	How to print math in HTML
writerIgnoreNotes :: Bool	Ignore footnotes (used in making toc)
writerIncremental :: Bool	Incremental S5 lists
writerNumberSections :: Bool	Number sections in LaTeX
writerIncludeBefore :: String	String to include before the body
writerIncludeAfter :: String	String to include after the body
writerStrictMarkdown :: Bool	Use strict markdown syntax
writerReferenceLinks :: Bool	Use reference links in writing markdown, rst
writerWrapText :: Bool	Wrap text to line length
writerLiterateHaskell :: Bool	Write as literate haskell
writerEmailObfuscation :: ObfuscationMethod	How to obfu

>   {-ex <- liftIO $ system ("pandoc -s -f markdown -t html "
>                          ++ src ++ " -o " ++ tgt)
>   case ex of
>     ExitFailure e -> throwError $ AEMisc $ "psql failed with " ++ show e
>     ExitSuccess -> return ()-}

================================================================================

process doc commands

> -- | read a text file, and pull out the commands, run them and insert
> --   the results into the text
> hsTextize :: MonadIO m => String -> ErrorT String m String
> hsTextize s =
>     liftIO (hsTextify
>              (("hssqlsystem", hsSqlSystemCommand):defaultCommands)
>              "docs/build"
>              s) >>= tsl -- . mapLeft 

> -- | run hssqlsystem using shell
> hsSqlSystemCommand :: String -> IO String
> hsSqlSystemCommand s =  shell ("HsSqlSystem " ++ s) >>= \m ->
>                         return $ "$ HsSqlSystem " ++ s
>                                  ++ "\n\n~~~~~~~~~~\n"
>                                  ++ m
>                                  ++ "\n~~~~~~~~~~\n\n"

================================================================================

> wrapperGen1 :: (MonadIO m, Error e) => String -> String -> ErrorT e m String
> wrapperGen1 db fn = liftIO $ wrapperGen db fn


================================================================================

errort stuff

wrap all our errors in an algebraic data type, not sure if there is a
more elegant way of doing this but it does the job for now

> {-data AllErrors = AEExtendedError ParseErrorExtra
>                | AETypeErrors [TypeError]
>                | AEMisc String
>                  deriving (Show)

> instance Error AllErrors where
>   noMsg = AEMisc "Unknown error"
>   strMsg = AEMisc

> throwEEEither :: (MonadError AllErrors m) => Either ParseErrorExtra a -> m a
> throwEEEither = throwEither . mapLeft AEExtendedError

> throwTESEither :: (MonadError AllErrors m) => Either [TypeError] a -> m a
> throwTESEither = throwEither . mapLeft AETypeErrors

> throwEither :: (MonadError t m) => Either t a -> m a
> throwEither (Left err) = throwError err
> throwEither (Right val) = return val-}


================================================================================

read file as string - issues are:

want to support reading from stdin, and reading from a string passed
as an argument to the exe

> -- | read a file as text, will read from stdin if filename is '-'.
> readInput :: (Error e, MonadIO m) => FilePath -> ErrorT e m String
> readInput f =
>   liftIO $ case f of
>              "-" -> getContents
>              _ | length f >= 2 &&
>                  head f == '"' && last f == '"'
>                    -> return $ drop 1 $ take (length f - 1) f
>                | otherwise -> readFile f

================================================================================

 > -- | write text to a file
 > writeFile :: (Error e, MonadIO m) => FilePath -> String -> ErrorT e m ()
 > writeFile fn =
 >     liftIO . System.IO.writeFile fn

================================================================================

Utilities

 > -- | wrapper for putstrln
 > message :: MonadIO m => String -> m ()
 > message = liftIO . putStrLn

 > -- | print a list, using newlines instead of commas, no outer []
 > printList :: (MonadIO m, Show a) => [a] -> m ()
 > printList = mapM_ (liftIO . print)

 > -- | run putstrln over each element of a list
 > putStrLnList :: MonadIO m => [String]-> m ()
 > putStrLnList = mapM_ (liftIO . putStrLn)

 > -- | lifted fst
 > lfst :: (Monad m, Error e) => (a,b) -> ErrorT e m a
 > lfst = return . fst

 > -- | lifted snd
 > lsnd :: (Monad m, Error e) => (a,b) -> ErrorT e m b
 > lsnd = return . snd

 > -- | lifted concat
 > lconcat :: (Monad m, Error e) => [[a]] -> ErrorT e m [a]
 > lconcat = return . concat


