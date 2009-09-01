#! /usr/bin/env runhaskell

command line is
./HsSqlSystem.lhs [commandName] [commandArgs ...]

commands are:
loadsql
cleardb
clearandloadsql
lexfile
showfileatts
checkppp
roundtrip

command args:

loadsql [databasename] [filename]*
database must already exist, loads sql from files into database, via
parsing, checking and pretty printing

cleardb [databasename]
attempts to reset the database to empty, using a hack

clearandloadsql [databasename] [filename]*
runs cleardb then loadsql

lexfile [filename]
lexes the file given then displays each token on a separate line

showfileatts [filename]
parses then runs the attribute grammar processor over the ast,
displays all the values produced

checkppp [filename]
parses then pretty prints then parses the pretty printed output. Used
to check a file can parse, and that pretty printing then parsing gives
you the same ast.

roundtrip [filename] [targetfilename]

getfntables [databasename]
output function and operator tables for the parser and type checker
from the given database


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

================================================================================

= main

> main :: IO ()
> main = do
>   --do this to avoid having to put flushes everywhere when we
>   --provide "..." progress thingys, etc.
>   hSetBuffering stdout NoBuffering
>   args <- getArgs
>   when (length args == 0) $ error "no command given"
>   case () of
>     _ | (length args == 2 && head args == "cleardb") -> cleardb (args !! 1)
>       | (length args >= 3 && head args == "loadsql") -> loadsqlfiles args
>       | (length args >= 3 && head args == "clearandloadsql") ->
>            cleardb (args !! 1) >> loadsqlfiles args
>       | (length args == 2 && head args == "lexfile") -> lexFile (args !! 1)
>       | (length args >= 2 && head args == "showfileatts") -> showfileatts (tail args)
>       | (length args >= 2 && head args == "parsefile") -> parseFile (tail args)
>       | (length args == 3 && head args == "roundtrip") -> roundTripFile (tail args)
>       | (length args == 2 && head args == "getfntables") -> getFnTables (args !! 1)
>       | otherwise -> error "couldn't parse command line"
>   where
>     loadsqlfiles args = mapM_ (loadSqlfile (args !! 1)) (tail $ tail args)


================================================================================

= load sql file

This takes a file full of sql from the disk and loads it into the
database given.

> loadSqlfile :: String -> String -> IO ()
> loadSqlfile db fn = do
>   res <- parseSqlFileWithState fn
>   case res of
>     Left er -> error $ show er
>     Right ast -> putStrLn ("loading " ++ fn)
>                  >> loadIntoDatabase db fn ast

================================================================================

= small hack utility to help with testing

TODO: use the correct username in this command
TODO: do something more correct

> cleardb :: String -> IO ()
> cleardb db = do
>   withConn ("dbname=" ++ db) $ \conn -> do
>     runSqlCommand conn "drop owned by jake cascade;"
>   putStrLn $ "database " ++ db ++ " cleared."


================================================================================

> lexFile :: FilePath -> IO ()
> lexFile f = do
>   putStrLn $ "lexing " ++ show f
>   x <- lexSqlFile f
>   return ()
>   case x of
>        Left er -> print er
>        Right l -> mapM_ print l

================================================================================

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

Routine to parse sql from a file, check that it appears to parse ok,
that pretty printing it and parsing that text gives the same ast,
and then displays the pretty printed version so you can see how well it's
done (maybe it could interpolate each original statement with its
parsed, pretty printed version so you can more easily check how
authentic the sql is and how much has been silently dropped on the floor

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

Used to test the parsing and pretty printing round trip. Takes two
arguments, a source filename and a target filename. If the target file
exists, it quits. Parses the source file then pretty prints it to the
target filename.

> roundTripFile :: [FilePath] -> IO ()
> roundTripFile args = do
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

getFnTables

read the operators and functions from the catalog of the given database
output four values: binops, prefixops, postfixops, functions
each is a list with type ({functionName} String
                         ,{args} [Type]
                         ,{retType} Type)

> getFnTables :: [Char] -> IO ()
> getFnTables dbName = withConn ("dbname=" ++ dbName) $ \conn -> do
>    let binopquery = "select oprname,\n\
>                     \       pg_catalog.format_type(oprleft, null),\n\
>                     \       pg_catalog.format_type(oprright, null),\n\
>                     \       pg_catalog.format_type(oprresult, null)\n\
>                     \  from pg_operator\n\
>                     \  where oprleft <> 0 and oprright <> 0\n\
>                     \  order by oprname;"
>    binopinfo <- selectRelation conn binopquery []
>    putStrLn $ makeVal "binaryOperatorTypes" $ map convBinopRow binopinfo
>    prefixopinfo <- selectRelation conn
>                      "select oprname,\n\
>                      \       pg_catalog.format_type(oprright, null),\n\
>                      \       pg_catalog.format_type(oprresult, null)\n\
>                      \  from pg_operator\n\
>                      \  where oprleft = 0\n\
>                      \  order by oprname;" []
>    putStrLn $ makeVal "prefixOperatorTypes" $ map convUnopRow prefixopinfo
>    postfixopinfo <- selectRelation conn
>                      "select oprname,\n\
>                      \       pg_catalog.format_type(oprleft, null),\n\
>                      \       pg_catalog.format_type(oprresult, null)\n\
>                      \  from pg_operator\n\
>                      \  where oprright = 0\n\
>                      \  order by oprname;" []
>    putStrLn $ makeVal "postfixOperatorTypes" $ map convUnopRow postfixopinfo
>    where
>      convBinopRow :: [String] -> String
>      convBinopRow l = "(" ++ stringIt (head l) ++ ","
>                       ++ toScalarTypes (take 2 $ drop 1 l) ++ ","
>                       ++ toScalarType (l !! 3) ++ ")"
>      convUnopRow l = "(" ++ stringIt (head l) ++ ","
>                       ++ toScalarTypes [l !! 1] ++ ","
>                       ++ toScalarType (l !! 2) ++ ")"
>      toScalarType s = "ScalarType " ++ stringIt s
>      toScalarTypes :: [String] -> String
>      toScalarTypes ss = "[" ++ intercalate ","  (map toScalarType ss) ++ "]"
>      stringIt s = "\"" ++ replace "\"" "\\\"" s ++ "\""
>      makeVal nm rows = nm ++ " = [\n    "
>                        ++ intercalate ",\n    " rows
>                        ++ "\n    ]"


> replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
> replace _ _ [] = []
> replace old new xs@(y:ys) =
>   case stripPrefix old xs of
>     Nothing -> y : replace old new ys
>     Just ys' -> new ++ replace old new ys'
