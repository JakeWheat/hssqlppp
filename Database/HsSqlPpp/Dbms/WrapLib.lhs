Copyright 2010 Jake Wheat

> {-# OPTIONS_HADDOCK hide #-}

> module Database.HsSqlPpp.Dbms.WrapLib where

> import Database.HDBC

> selectRelation :: (IConnection conn) =>
>                   conn -> String -> [SqlValue] -> IO [[SqlValue]]
> selectRelation conn query args = do
>   sth <- prepare conn query
>   execute sth args
>   v <- fetchAllRows' sth
>   return v

