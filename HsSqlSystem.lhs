#! /usr/bin/env runhaskell

Copyright 2009 Jake Wheat

Command line access to a bunch of utility functions.

command line is
./HsSqlSystem.lhs [commandName] [commandArgs ...]

run
./HsSqlSystem.lhs help
to get a list of commands and purpose and usage info

TODO 1: add options to specify username and password (keep optional though)
TODO 2: think of a name for this command

> import System
> import System.IO
> import Control.Monad
> import System.Directory
> import Data.List
> import Control.Applicative
> import Data.Either

> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Parsing.Lexer

> import Database.HsSqlPpp.Ast.Ast
> import Database.HsSqlPpp.Ast.Annotator
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Ast.Environment

> import Database.HsSqlPpp.PrettyPrinter.PrettyPrinter
> import Database.HsSqlPpp.PrettyPrinter.AnnotateSource

> import Database.HsSqlPpp.Dbms.DBAccess
> import Database.HsSqlPpp.Dbms.DatabaseLoader

================================================================================

= main

> main :: IO ()
> main = do
>   args <- getArgs
>   case () of
>        _ | null args -> putStrLn "no command given" >> help []
>          | otherwise -> case lookupCaller commands (head args) of
>                           Nothing -> putStrLn "unrecognised command" >> help []
>                           Just c -> call c $ tail args

> commands :: [CallEntry]
> commands = [helpCommand
>            ,clearDBCommand
>            ,loadSqlCommand
>            ,clearAndLoadSqlCommand
>            ,lexFileCommand
>            ,parseFileCommand
>            ,roundTripCommand
>            ,readEnvCommand
>            ,annotateSourceCommand
>            ,showInfoCommand
>            ,showInfoDBCommand]

> lookupCaller :: [CallEntry] -> String -> Maybe CallEntry
> lookupCaller ce name = find (\(CallEntry nm _ _) -> name == nm) ce

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

================================================================================

= load sql file

> loadSqlCommand :: CallEntry
> loadSqlCommand = CallEntry
>                  "loadsql"
>                  "This takes one or more files with sql source text, \
>                  \parses them then loads them into the database given."
>                  (Multiple loadSql)

> loadSql :: [String] -> IO ()
> loadSql args = do
>   -- do this to avoid having to put flushes everywhere when we
>   -- provide "..." progress thingys, etc..
>   hSetBuffering stdout NoBuffering
>   let (db:fns) = args
>   forM_ fns $ \fn -> do
>   res <- parseSqlFile fn
>   case res of
>     Left er -> error $ show er
>     Right ast -> putStrLn ("loading " ++ fn)
>                  >> loadIntoDatabase db fn ast

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
> cleardb db = do
>   withConn ("dbname=" ++ db) $ \conn ->
>     runSqlCommand conn "drop owned by jake cascade;"
>   putStrLn $ "database " ++ db ++ " cleared."

================================================================================

> clearAndLoadSqlCommand :: CallEntry
> clearAndLoadSqlCommand = CallEntry
>                          "clearandloadsql"
>                          "cleardb then loadsql"
>                          (Multiple
>                           (\args -> do
>                              cleardb $ head args
>                              loadSql args))

================================================================================

> lexFileCommand :: CallEntry
> lexFileCommand = CallEntry
>                  "lexfile"
>                  "lex the file given and output the tokens on separate lines"
>                  (Single lexFile)
> lexFile :: FilePath -> IO ()
> lexFile f = do
>   putStrLn $ "lexing " ++ show f
>   x <- lexSqlFile f
>   return ()
>   case x of
>        Left er -> print er
>        Right l -> mapM_ print l

================================================================================

> parseFileCommand :: CallEntry
> parseFileCommand = CallEntry
>                    "parsefile"
>                    "Routine to parse sql from a file, check that it appears to parse ok, \
>                    \that pretty printing it and parsing that text gives the same ast, \
>                    \and then displays the pretty printed version so you can see how well it's \
>                    \done"
>                    --(maybe it could interpolate each original statement with its
>                    --parsed, pretty printed version so you can more easily check how
>                    --authentic the sql is and how much has been silently dropped on the floor)
>                    (Multiple parseFile)

> parseFile :: [String] -> IO ()
> parseFile = mapM_ pf
>   where
>     pf f = do
>       putStrLn $ "parsing " ++ show f
>       x <- parseSqlFile f
>       case x of
>            Left er -> print er
>            Right st -> do
>                --print l
>                --putStrLn "END OF AST END OF AST END OF AST END OF AST END OF AST END OF AST"
>                putStrLn "parse ok"
>                print st
>                let pp = printSql st
>                --putStrLn pp
>                --check roundtrip
>                case parseSql pp of
>                  Left er -> error $ "roundtrip failed: " ++ show er
>                  Right st' -> if map stripAnnotations st == map stripAnnotations st'
>                                then putStrLn "roundtrip ok"
>                                else putStrLn "roundtrip failed: different ast"
>       return ()

================================================================================

> showInfoCommand :: CallEntry
> showInfoCommand = CallEntry
>                    "showinfo"
>                    "reads each file, parses, type checks, then outputs info on each statement \
>                    \interspersed with the pretty printed statements"
>                    (Multiple showInfo)

> showInfo :: [FilePath] -> IO ()
> showInfo = showInfoEnv defaultTemplate1Environment

> showInfoEnv :: Environment -> [FilePath] -> IO ()
> showInfoEnv env fs = do
>   astEithers <- mapM parseSqlFile fs
>   let asts = rights astEithers
>   let aasts = annotateAstsEnv env asts
>   mapM_ print $ lefts astEithers
>   mapM_ printAst aasts
>   where
>     printAst :: StatementList -> IO ()
>     printAst = mapM_ (putStrLn . printSqlAnn annotToS . (:[]))
>     annotToS :: Annotation -> String
>     annotToS = concat . intersperse "\n" . map show



================================================================================

> showInfoDBCommand :: CallEntry
> showInfoDBCommand = CallEntry
>                    "showinfodb"
>                    "pass the name of a database first, then \
>                    \filenames, reads each file, parses, type checks, \
>                    \then outputs info on each statement interspersed with the \
>                    \pretty printed statements, will type check \
>                    \against the given database schema"
>                    (Multiple showInfoDB)


> showInfoDB :: [FilePath] -> IO ()
> showInfoDB (dbName:fs) = do
>   env <- updateEnvironment defaultEnvironment <$> readEnvironmentFromDatabase dbName
>   case env of
>     Left e -> error $ show e
>     Right e1 ->
>         showInfoEnv e1 fs
> showInfoDB _ = error "please pass the database name plus at least one filename"

================================================================================

> annotateSourceCommand :: CallEntry
> annotateSourceCommand = CallEntry
>                    "annotateSource"
>                    "reads each file, parses, type checks, then outputs info on each statement \
>                    \interspersed with the original source code"
>                    (Single annotateSourceF)

> annotateSourceF :: FilePath -> IO ()
> annotateSourceF f = do
>   aste <- parseSqlFile f
>   case aste of
>     Left er -> error $ show er
>     Right ast -> do
>                  src <- readFile f
>                  let aast = annotateAst ast
>                      srcnew = annotateSource src aast
>                  putStr srcnew



================================================================================

> roundTripCommand :: CallEntry
> roundTripCommand = CallEntry
>                        "roundtripfile"
>                        "Used to test the parsing and pretty printing round trip. Takes two \
>                        \arguments, a source filename and a target filename. If the target file \
>                        \exists, it quits. Parses the source file then pretty prints it to the \
>                        \target filename."
>                        (Multiple roundTrip)

> roundTrip :: [FilePath] -> IO ()
> roundTrip args = do
>   when (length args /= 2) $
>          error "Please pass exactly two filenames, source and target."
>   let (source:target:[]) = args
>   targetExists <- doesFileExist target
>   when targetExists $
>          error "the target file name exists already, please delete it or choose a new filename"
>   x <- parseSqlFile source
>   case x of
>        Left er -> print er
>        Right l -> writeFile target $ printSql l

================================================================================

This reads an environment from a database and writes it out using show.

> readEnvCommand :: CallEntry
> readEnvCommand = CallEntry
>                   "readenv"
>                   "read the catalogs for the given db and dump a Environment value source text to stdout"
>                   (Single readEnv)
> readEnv :: String -> IO ()
> readEnv dbName = do
>   s <- readEnvironmentFromDatabase dbName
>   putStr "\n\
>          \Copyright 2009 Jake Wheat\n\
>          \\n\
>          \This file contains\n\
>          \\n\
>          \> {-# OPTIONS_HADDOCK hide  #-}\n\
>          \\n\
>          \> module Database.HsSqlPpp.AstInternals.DefaultTemplate1Environment\n\
>          \>     (defaultTemplate1Environment\n\
>          \>      ) where\n\
>          \\n\
>          \> import Database.HsSqlPpp.AstInternals.EnvironmentInternal\n\
>          \> import Database.HsSqlPpp.AstInternals.TypeType\n\
>          \\n\
>          \> defaultTemplate1Environment :: Environment\n\
>          \> defaultTemplate1Environment = (\\l -> case l of\n\
>          \>                                       Left x -> error $ show x\n\
>          \>                                       Right e -> e)\n\
>          \>                                 $ updateEnvironment defaultEnvironment "

>   print s

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
