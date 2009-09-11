Copyright 2009 Jake Wheat

This file contains a few (almost pointlessly) lightweight wrappers
around hdbc for running commands and queries.

> module DBAccess (runSqlCommand
>                 ,withConn
>                 ,selectValue
>                 ,catchSql
>                 ,seErrorMsg
>                 ,selectRelation) where

> import qualified Database.HDBC.PostgreSQL as Pg
> import Database.HDBC
> import Control.Monad
> import Control.Exception

> runSqlCommand :: (IConnection conn) =>
>           conn -> String -> IO ()
> runSqlCommand conn query {-args-} = do
>     run conn query [] {- $ map toSql args-}
>     commit conn

> withConn :: String -> (Pg.Connection -> IO c) -> IO c
> withConn cs = bracket (Pg.connectPostgreSQL cs) disconnect

> selectValue :: (IConnection conn) =>
>                conn -> String -> IO String
> selectValue conn query = do
>   r <- quickQuery' conn query [] -- $ map sToSql args
>   case length r of
>     0 -> error $ "select value on " ++ query ++
>                         " returned 0 rows, expected 1"
>     1 -> do
>       let t = head r
>       when (length t /= 1)
>         (error $ "select value on " ++ query ++
>              " returned " ++ show (length t) ++ " attributes, expected 1.")
>       return $ toS $ head t
>     _ -> error $ "select value on " ++ query ++
>              " returned " ++ show (length r) ++ " tuples, expected 0 or 1."
>   where
>     toS a = fromSql a :: String

> selectRelation ::(IConnection conn) =>
>                  conn -> String -> [String] -> IO [[String]]
> selectRelation conn query args = do
>   sth <- prepare conn query
>   execute sth $ map sToSql args
>   v <- fetchAllRows' sth
>   return $ map (map sqlToS) v

> sToSql :: String -> SqlValue
> sToSql s = toSql (s::String)

> sqlToS :: SqlValue -> String
> sqlToS = fromSql
