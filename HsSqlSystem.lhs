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

> import Parser
> import DatabaseLoader
> import Lexer
> import Ast
> import PrettyPrinter
> import DBAccess
> import ScopeReader

================================================================================

= main

> main :: IO ()
> main = do
>   --do this to avoid having to put flushes everywhere when we
>   --provide "..." progress thingys, etc.
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
>            ,showFileAttsCommand
>            ,parseFileCommand
>            ,roundTripCommand
>            ,getScopeCommand
>            ,showTypesCommand
>            ,showTypesDBCommand]

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
>                  "This takes one or more files with sql source txt database given."
>                  (Multiple loadSql)

> loadSql :: [String] -> IO ()
> loadSql args =
>   let (db:fns) = args
>   in forM_ fns $ \fn -> do
>   res <- parseSqlFileWithState fn
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

> showFileAttsCommand :: CallEntry
> showFileAttsCommand = CallEntry
>                       "showFileAtts"
>                       "run the ag over the given files and return all the attributes computed"
>                       (Multiple showfileatts)

> showfileatts :: [String] -> IO ()
> showfileatts = mapM_ pf
>   where
>     pf f = do
>       putStrLn $ "parsing " ++ show f
>       x <- parseSqlFileWithState f
>       case x of
>            Left er -> print er
>            Right st -> do
>                mapM_ print st
>                putStrLn "\nchecking ast"
>                let y = checkAst st
>                print y
>       return ()

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
>       x <- parseSqlFileWithState f
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
>                  Right st' -> if resetSps' st == resetSps' st'
>                                then putStrLn "roundtrip ok"
>                                else putStrLn "roundtrip failed: different ast"
>       return ()

================================================================================

> showTypesCommand :: CallEntry
> showTypesCommand = CallEntry
>                    "showtypes"
>                    "reads each file, parses, type checks, then outputs the types \
>                    \interspersed with the pretty printed statements"
>                    -- todo: modify this so that is inserts the types as comments into the
>                    -- original source, get it to work correctly when run multiple times or
>                    -- the types have changed between runs (i.e. add or update the comments)
>                    (Multiple showTypes)

> showTypes :: [FilePath] -> IO ()
> showTypes = mapM_ pt
>   where
>     pt f = do
>       x <- parseSqlFileWithState f
>       case x of
>            Left er -> print er
>            Right sts -> do
>                let types = zip (getStatementsType sts) sts
>                mapM_ (\(t,st) -> putStrLn ("/*\n" ++ show t ++ "*/\n") >>
>                                  putStrLn (printSql [st])) types

================================================================================

> showTypesDBCommand :: CallEntry
> showTypesDBCommand = CallEntry
>                    "showtypesdb"
>                    "pass the name of a database first, then \
>                    \filenames, reads each file, parses, type checks, \
>                    \then outputs the types interspersed with the \
>                    \pretty printed statements, will type check \
>                    \against the given database schema"
>                    (Multiple showTypesDB)


> showTypesDB :: [FilePath] -> IO ()
> showTypesDB args = do
>   let dbName = head args
>   scope <- readScope dbName
>   mapM_ (pt scope) $ tail args
>   where
>     pt scope f = do
>       x <- parseSqlFileWithState f
>       case x of
>            Left er -> print er
>            Right sts -> do
>                let types = zip (getStatementsTypeScope scope sts) sts
>                mapM_ (\(t,st) -> putStrLn ("/*\n" ++ show t ++ "*/\n") >>
>                                  putStrLn (printSql [st])) types


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

> getScopeCommand :: CallEntry
> getScopeCommand = CallEntry
>                   "getscope"
>                   "read the catalogs for the given db and dump a scope variable source text to stdout"
>                   (Single getScope)
> getScope :: String -> IO ()
> getScope dbName = do
>   s <- readScope dbName
>   putStrLn "module DefaultScope where"
>   putStrLn "import Data.Map"
>   putStrLn "import TypeType"
>   putStrLn "import Scope"
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
