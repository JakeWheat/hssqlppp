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

> {-# LANGUAGE ScopedTypeVariables #-}

> import System
> import System.IO
> import System.Directory
> import Data.List
> import Data.Either
> import Control.Applicative
> import Text.Show.Pretty
> import Control.Monad.Error
> --import Control.Monad

> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Parsing.Lexer

> import Database.HsSqlPpp.Ast.Annotator
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Ast.Environment
> import Database.HsSqlPpp.Ast.Ast

> import Database.HsSqlPpp.Utils

> import Database.HsSqlPpp.PrettyPrinter.PrettyPrinter
> import Database.HsSqlPpp.PrettyPrinter.AnnotateSource

> import Database.HsSqlPpp.Dbms.DBAccess
> import Database.HsSqlPpp.Dbms.DatabaseLoader

> import Database.HsSqlPpp.Extensions.ChaosExtensions

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
>            ,checkSourceCommand
>            ,checkSourceExtCommand
>            ,checkBigCommand]

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
>                putStrLn $ ppShow st
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

> annotateSourceCommand :: CallEntry
> annotateSourceCommand = CallEntry
>                    "annotateSource"
>                    "reads a file, parses, type checks, then outputs info on each statement \
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
>                      srcnew = annotateSource False src aast
>                  putStr srcnew

================================================================================

> checkSourceCommand :: CallEntry
> checkSourceCommand = CallEntry
>                    "checksource"
>                    "reads each file, parses, type checks, then outputs any type errors"
>                    (Multiple checkSource)

> checkSource :: [FilePath] -> IO ()
> checkSource fns = do
>   astEithers <- mapM parseSqlFile fns
>   let asts = rights astEithers
>   let aasts = annotateAstsEnv defaultTemplate1Environment asts
>   mapM_ print $ lefts astEithers
>   mapM_ showTes $ aasts
>   where
>     showTes = mapM_ (putStrLn.showSpTe) . getTypeErrors
>     showSpTe (Just (SourcePos fn l c), e) =
>         fn ++ ":" ++ show l ++ ":" ++ show c ++ ":\n" ++ show e
>     showSpTe (_,e) = "unknown:0:0:\n" ++ show e

================================================================================

> checkSourceExtCommand :: CallEntry
> checkSourceExtCommand = CallEntry
>                    "checksourceext"
>                    "reads each file, parses, runs extensions, type checks, then outputs any type errors"
>                    (Multiple checkSourceExt)

> checkSourceExt :: [FilePath] -> IO ()
> checkSourceExt (dbName:fns) = do
>   hSetBuffering stdout NoBuffering
>   hSetBuffering stderr NoBuffering
>   env1 <- updateEnvironment defaultEnvironment <$> readEnvironmentFromDatabase dbName
>   let env = case env1 of
>               Left e -> error $ show e
>               Right e1 -> e1
>   astEithers <- mapM parseSqlFile fns
>   let asts = map extensionize $ rights astEithers
>   let aasts = annotateAstsEnv env asts
>   mapM_ print $ lefts astEithers
>   mapM_ showTes $ aasts
>   where
>     showTes = mapM_ (putStrLn.showSpTe) . getTypeErrors
>     showSpTe (Just (SourcePos fn l c), e) =
>         fn ++ ":" ++ show l ++ ":" ++ show c ++ ":\n" ++ show e
>     showSpTe (_,e) = "unknown:0:0:\n" ++ show e
> checkSourceExt _ = error "checksourceext not passed at least 2 args"


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
>          \> module Database.HsSqlPpp.AstInternals.Environment.DefaultTemplate1Environment\n\
>          \>     (defaultTemplate1Environment\n\
>          \>      ) where\n\
>          \\n\
>          \> import Database.HsSqlPpp.AstInternals.Environment.EnvironmentInternal\n\
>          \> import Database.HsSqlPpp.AstInternals.TypeType\n\
>          \\n\
>          \> defaultTemplate1Environment :: Environment\n\
>          \> defaultTemplate1Environment =\n\
>          \>    (\\l -> case l of\n\
>          \>             Left x -> error $ show x\n\
>          \>             Right e -> e) $\n\
>          \>     updateEnvironment defaultEnvironment\n"

>   putStrLn $ unlines $ map (">        " ++) $ lines $ ppShow s


> parseAndTypeCheck :: String
>                   -> String
>                   -> IO StatementList
> parseAndTypeCheck dbName src = do
>    case parseSql src of
>      Left e -> error $ show e
>      Right ast -> do
>        e <- updateEnvironment defaultEnvironment <$> readEnvironmentFromDatabase dbName
>        case e of
>          Left er -> error $ show er
>          Right env -> return $ annotateAstEnv env ast

================================================================================

big set of tests to check parsing, typechecking, the catalog, etc.

parse, run extensions and type check the sql files passed against the database passed (which should be a copy of template1 - maybe enforce this)
save this aast and final catalog from type checker-> original_ast,original_catalog
load the original sql files into the database using psql
use readenv to read the catalog from this database, and compare with the original_catalog for equality
use pg_dump on this database, reparse and typecheck-> d1_ast, d1_catalog
check these for equality with the original_ast,catalog (will have to cope with non important differences, hopefully only reordered statements)
take the original_ast, reset database and load this ast into the database using the database loader
get catalog and dump and compare for equality with originals

> checkBigCommand :: CallEntry
> checkBigCommand = CallEntry
>                    "checkbig"
>                    "reads each file, and gets catalog updates using type checker,\
>                     \ then loads the files into the database and reads the catalog\
>                     \ from the database and checks it for consistency with the\
>                     \ catalog determined by the type checker"
>                    (Multiple checkBig)

> doEithers :: [Either a b] -> Either a [b]
> doEithers es = let l = lefts es
>                in if null l
>                   then Right $ rights es
>                   else Left $ head l

> checkBig :: [FilePath] -> IO ()
> checkBig (dbName:fns) = do
>     hSetBuffering stdout NoBuffering
>     hSetBuffering stderr NoBuffering
>     r <- runErrorT runit
>     case r of
>            Left e -> error e
>            Right _ -> return ()
>     where
>       runit = do
>         message $ "clearing " ++ dbName
>         liftIO $ cleardb dbName

>         message "parsing"
>         (ast::StatementList) <- liftIO parseFiles >>= liftThrows
>         let east = extensionize ast
>         (startingEnv::Environment) <- liftIO readDbEnv >>= liftThrows
>         -- type check ast and get catalog

>         message "typechecking"
>         let (originalEnv,originalAast) = annotateAstEnvEnv startingEnv east
>         -- quit if any type check errors
>         let te = getTypeErrors originalAast
>         --when (not $ null te) $ throwError $ intercalate "\n" $ map showSpTe te
>         message $ intercalate "\n" $ map showSpTe te

>         message "loading into db using psql"
>         liftIO $ mapM (runSqlScript dbName) fns
>         properEnv <- liftIO readDbEnv >>= liftThrows

>         let startEnvBits = deconstructEnvironment startingEnv
>             originalEnvBits = deconstructEnvironment originalEnv \\ startEnvBits
>             properEnvBits = deconstructEnvironment properEnv \\ startEnvBits
>             missing = sort $ properEnvBits \\ originalEnvBits
>             extras = sort $ originalEnvBits \\ properEnvBits
>         liftIO $ when (not $ null missing)
>                    $ putStrLn $ "\n\n************************************************\n\n\
>                                 \missing catalog: " ++ showAList missing
>         liftIO $ when (not $ null extras)
>                    $ putStrLn $ "\n\n************************************************\n\n\
>                                 \extras catalog: " ++ showAList extras

>         --liftIO $ putStrLn $ "\n\n************************************************\n\n\
>         --                    \common: " ++ showAList (sort $ intersect properEnvBits originalEnvBits)

>         message "complete!"
>         return $ Right ()

>       showAList l = intercalate "\n" $ map show l

>       parseFiles :: IO (Either String StatementList)
>       parseFiles = mapEither show concat <$> doEithers <$> mapM parseSqlFile fns
>       --startingEnv :: IO (Either String Environment)
>       readDbEnv = mapLeft show <$> (updateEnvironment defaultEnvironment <$> readEnvironmentFromDatabase dbName)

>       --showTes = mapM_ (putStrLn.showSpTe) . getTypeErrors
>       showSpTe (Just (SourcePos fn l c), e) =
>         fn ++ ":" ++ show l ++ ":" ++ show c ++ ":\n" ++ show e
>       showSpTe (_,e) = "unknown:0:0:\n" ++ show e
>       message = liftIO . putStrLn
> checkBig _ = error "checkbig not passed at least 2 args"


> runSqlScript :: String -> String -> IO ()
> runSqlScript dbName script = do
>   ex <- system ("psql " ++ dbName ++
>                 " -q --set ON_ERROR_STOP=on" ++
>                 " --file=" ++ script)
>   case ex of
>     ExitFailure e -> error $ "psql failed with " ++ show e
>     ExitSuccess -> return ()
>   return ()



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

