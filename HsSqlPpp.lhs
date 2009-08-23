#! /usr/bin/env runhaskell

command line:

./HsSqlPpp.lhs loadsql [databasename] [filename]*
database must already exist

./HsSqlPpp.lhs cleardb [databasename]
attempts to reset the database to empty

./HsSqlPpp.lhs clearandloadsql [databasename] [filename]*
runs cleardb then loadsql


TODO1: add options to specify username and password (keep optional though)
TODO 2: think of a name for this command

> import System
> import System.IO
> import Control.Monad

> import Parser
> import DatabaseLoader

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
>       | otherwise -> error "couldn't parse command line"
>   where
>     loadsqlfiles args = mapM_ (loadSqlfile (args !! 1)) (tail $ tail args)

= small hack utility to help with testing

TODO: use the correct username in this command
TODO: do something more correct

> cleardb :: String -> IO ()
> cleardb db = do
>   withConn ("dbname=" ++ db) $ \conn -> do
>     runSqlCommand conn "drop owned by jake cascade;"
>   putStrLn $ "database " ++ db ++ " cleared."

