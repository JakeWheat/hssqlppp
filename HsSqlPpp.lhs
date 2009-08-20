#! /usr/bin/env runhaskell

command line:

./HsSqlPpp.lhs loadsql [databasename] [filename]
database must already exist

./HsSqlPpp.lhs cleardb [databasename]
attempts to reset the database to empty

TODO1: add options to specify username and password (make optional though)

> import System
> import System.IO
> import qualified Database.HDBC.PostgreSQL as Pg
> import Database.HDBC
> import Control.Monad
> import Control.Exception

> import Parser
> import PrettyPrinter

================================================================================

= load sql file

This takes a file full of sql from the disk and loads it into the
database named. It does this by parsing the sql file, then pretty
printing each Statement from the ast in turn and then pretty printing
it and sending it to the database. (There is no fundamental reason why
it loads each statement one at a time instead of pretty printing the
whole lot, just that the run commmand from hdbc one supports one
command at a time.)

The next todo is to start adding token position information to the
ast, Then when error (and notice, etc.) messages come back from the
database when loading the sql, can show the pretty printed sql with
the error pos highlighted, as well as providing the original position
from the sql file.

This seems like a whole lot of effort for nothing, but will then allow
using transforming the ast so it no longer directly corresponds to the
source sql, and loading the transformed sql into the database. For
generated code, some thought will be needed on how to link any errors
back to the original source text.

One option for some of the code is to use the original source code
when it hasn't been changed for a given statement, instead of the
pretty printed stuff, don't know how much help this will be.


> loadSqlfile :: String -> String -> IO ()
> loadSqlfile db fn = do
>   res <- parseSqlFile fn
>   case res of
>     Left er -> error $ show er
>     Right ast -> putStrLn ("loading " ++ fn)
>                  >> loadAst ast
>   where
>     loadAst ast = withConn ("dbname=" ++ db) $ \conn -> do
>                     -- first, check plpgsql is in the database
>                     x <- selectValue conn "select count(1) from pg_language where lanname='plpgsql';"
>                     when (read x == 0) $
>                          runSqlCommand conn "create procedural language plpgsql;"
>                     mapM_ (\st ->
>                            loadStatement conn st
>                            >> putStr ".") ast
>     loadStatement conn st =
>         runSqlCommand conn (printSql [st])

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
>       | (length args == 3 && head args == "loadsql") ->
>            loadSqlfile (args !! 1) (args !! 2)
>       | (length args == 3 && head args == "clearandloadsql") ->
>            cleardb (args !! 1) >> loadSqlfile (args !! 1) (args !! 2)
>       | otherwise -> error "couldn't parse command line"

= small hack utility to help with testing

TODO: use the correct username in this command
TODO: do something more correct

> cleardb :: String -> IO ()
> cleardb db = do
>   withConn ("dbname=" ++ db) $ \conn -> do
>     runSqlCommand conn "drop owned by jake cascade;"
>   putStrLn $ "database " ++ db ++ " cleared."

================================================================================

= database wrapper stuff

> runSqlCommand :: (IConnection conn) =>
>           conn -> String -> IO ()
> runSqlCommand conn query {-args-} = do
>     run conn query [] {- $ map toSql args-}
>     commit conn

> connect :: String -> IO Pg.Connection
> connect db = Pg.connectPostgreSQL db

> withConn :: String -> (Pg.Connection -> IO c) -> IO c
> withConn cs f = bracket (Pg.connectPostgreSQL cs)
>                         (\c -> disconnect c)
>                          f

  selectValue :: (IConnection conn) =>
                 conn -> String -> t -> IO String

> selectValue conn query = do
>   r <- quickQuery' conn query [] -- $ map sToSql args
>   case length r of
>     0 -> error $ "select value on " ++ query ++
>                         " returned 0 rows, expected 1"
>     1 -> do
>       let t = head r
>       when (length t /= 1)
>         (error $ "select value on " ++ query ++
>              " returned " ++ (show $ length t) ++ " attributes, expected 1.")
>       return $ toS $ head t
>     _ -> error $ "select value on " ++ query ++
>              " returned " ++ (show $ length r) ++ " tuples, expected 0 or 1."
>   where
>     toS a = (fromSql a)::String
