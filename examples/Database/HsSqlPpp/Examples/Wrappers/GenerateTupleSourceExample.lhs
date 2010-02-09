Copyright 2010 Jake Wheat

Example of using the tuple code generator.

> module Database.HsSqlPpp.Examples.Wrappers.GenerateTupleSourceExample
>     {-(runTest)-} where
>
> import Data.List
> import Database.HDBC
>
> --import Database.HsSqlPpp.Utils.DbmsCommon
> import Database.HsSqlPpp.Examples.Wrappers.ConnectionString
> --import Database.HsSqlPpp.Examples.Wrappers.ThTupleWrapper
>
> runTest :: IO String
> runTest =
>   -- the connection string here is used at runtime
>   withConn ("dbname=" ++ connectionString) $ \conn -> do
>   x <- suppliers conn
>   y <- goodSuppliers conn (Just 4)
>   return $ "suppliers\n" ++ showl x ++
>            "\nsuppliers 4\n" ++ showl y
>   where
>     showl = intercalate "\n" . map show


 > -- the connection string here is used at compile time to get the
 > -- type of the sql statement
 > suppliers :: IConnection conn => conn -> IO [(Maybe Int, Maybe String, Maybe Int, Maybe String)]

> suppliers = sqlQuery connectionString "select * from s;"

 > goodSuppliers :: IConnection conn => conn
 >               -> Maybe Int
 >               -> IO [(Maybe Int, Maybe String, Maybe Int, Maybe String)]

> goodSuppliers = sqlQuery connectionString "select * from s where status >= ?;"
