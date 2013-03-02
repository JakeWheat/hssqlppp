
wrapper around postgresql-simple

the only thing it does is provide a version of withDB which accepts a
connection string instead of a ConnectionInfo

> module Database.HsSqlPpp.Utils.PgUtils
>     (withConn
>     ,module Database.PostgreSQL.Simple) where

> import qualified Database.PostgreSQL.Simple as Pg
> import Database.PostgreSQL.Simple
> import Control.Exception
> import qualified Data.ByteString.Char8 as B

> withConn :: String -> (Pg.Connection -> IO c) -> IO c
> withConn cs = bracket (Pg.connectPostgreSQL $ B.pack cs) Pg.close
