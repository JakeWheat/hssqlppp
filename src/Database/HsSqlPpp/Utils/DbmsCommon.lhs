
Some simple wrappers around HDBC for the code to use.

> module Database.HsSqlPpp.Utils.DbmsCommon
>     (withConn
>     ,selectRelation
>     ,runSqlCommand
>     ,withTransaction
>     ,Pg.Connection
>     ,IConnection) where
>
> import qualified Database.HDBC.PostgreSQL as Pg
> import Database.HDBC
> import Control.Exception
>
> withConn :: String -> (Pg.Connection -> IO c) -> IO c
> withConn cs = bracket (Pg.connectPostgreSQL cs) disconnect
>
> selectRelation ::(IConnection conn) =>
>                  conn -> String -> [String] -> IO [[String]]
> selectRelation conn query args = do
>   sth <- prepare conn query
>   _ <- execute sth $ map sToSql args
>   v <- fetchAllRows' sth
>   return $ map (map sqlToS) v
>
> runSqlCommand :: (IConnection conn) =>
>           conn -> String -> IO ()
> runSqlCommand conn query = do
>     _ <- run conn query []
>     commit conn
>
> sToSql :: String -> SqlValue
> sToSql s = toSql (s::String)
>
> sqlToS :: SqlValue -> String
> sqlToS = fromSql
