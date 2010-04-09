Copyright 2010 Jake Wheat

> -- | Utility function for use with WrapperGen example code
> module Database.HsSqlPpp.Examples.Wrappers.WrapLib where
>
> import Database.HDBC
>
> selectRelation :: (IConnection conn) =>
>                   conn -> String -> [SqlValue] -> IO [[SqlValue]]
> selectRelation conn query args = do
>   sth <- prepare conn query
>   _ <- execute sth args
>   v <- fetchAllRows' sth
>   return v
