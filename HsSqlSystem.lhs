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

> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Dbms.DatabaseLoader
> import Database.HsSqlPpp.Parsing.Lexer
> import Database.HsSqlPpp.TypeChecking.Ast
> import Database.HsSqlPpp.PrettyPrinter.PrettyPrinter
> import Database.HsSqlPpp.Dbms.DBAccess
> import Database.HsSqlPpp.TypeChecking.Scope

================================================================================

= main

> main :: IO ()
> main = do
>   -- do this to avoid having to put flushes everywhere when we
>   -- provide "..." progress thingys, etc.. should be fixed so only
>   -- used in commands that need it
>   hSetBuffering stdout NoBuffering
>   args <- getArgs
>   case () of
>        _ | null args -> putStrLn "no command given" >> help False
>          | args == ["help"] -> help False
>          | args == ["help", "all"] -> help True
>          | head args == "help" -> helpCommand (args !! 1)
>          | otherwise -> case lookupCaller commands (head args) of
>                           Nothing -> putStrLn "unrecognised command" >> help False
>                           Just c -> call c $ tail args

> commands :: [CallEntry]
> commands = [clearDBCommand
>            ,loadSqlCommand
>            ,clearAndLoadSqlCommand
>            ,lexFileCommand
>            ,parseFileCommand
>            ,roundTripCommand
>            ,getScopeCommand
>            ,showInfoCommand
>            ,showInfoDBCommand]

> lookupCaller :: [CallEntry] -> String -> Maybe CallEntry
> lookupCaller ce name = find (\(CallEntry nm _ _) -> name == nm) ce

> help :: Bool -> IO ()
> help full = do
>   putStrLn "commands available"
>   putStrLn "use 'help' to see a list of commands"
>   putStrLn "use 'help all' to see a list of commands with descriptions"
>   putStrLn "use 'help [command]' to see the description for that command\n"
>   mapM_ putStrLn $ flip map commands (\(CallEntry nm desc _)  ->
>                                           if full
>                                             then nm ++ "\n" ++ desc ++ "\n"
>                                             else nm ++ "\n")

> helpCommand :: String -> IO ()
> helpCommand c =
>     case lookupCaller commands c of
>       Nothing -> putStrLn "unrecognised command" >> help False
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
> loadSql args =
>   let (db:fns) = args
>   in forM_ fns $ \fn -> do
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
> showInfo = mapM_ pt
>   where
>     pt f = do
>       x <- parseSqlFile f
>       case x of
>            Left er -> print er
>            Right sts -> do
>                let aast = annotateAst sts
>                mapM_ (putStrLn . printSqlAnn show . (:[])) aast

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
> showInfoDB args = do
>   let dbName = head args
>   scope <- readScope dbName
>   mapM_ (pt scope) $ tail args
>   where
>     pt scope f = do
>       x <- parseSqlFile f
>       case x of
>            Left er -> print er
>            Right sts -> do
>                let aast = annotateAstScope scope sts
>                mapM_ (putStrLn . printSqlAnn annotToS . (:[])) aast
>     annotToS :: Annotation -> String
>     annotToS a = concat $ intersperse "\n" $ map show a

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

This writes out the default scope using show, which you can then
compile. The output is all on one big line (something like 400,000
characters!) and compiles very slowly for the catalog of a standard
template1 database...

> getScopeCommand :: CallEntry
> getScopeCommand = CallEntry
>                   "getscope"
>                   "read the catalogs for the given db and dump a scope variable source text to stdout"
>                   (Single getScope)
> getScope :: String -> IO ()
> getScope dbName = do
>   s <- readScope dbName
>   putStrLn "{-# OPTIONS_HADDOCK hide #-}"
>   putStrLn "module Database.HsSqlPpp.TypeChecking.DefaultScope where"
>   putStrLn "import Database.HsSqlPpp.TypeChecking.TypeType"
>   putStrLn "import Database.HsSqlPpp.TypeChecking.ScopeData"
>   putStrLn "defaultScope :: Scope"
>   putStr "defaultScope = "
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
