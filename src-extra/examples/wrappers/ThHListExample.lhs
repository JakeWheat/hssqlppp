Example of using the template haskell hlist wrapper. Can't get it to
compile, some sort of type class issue related to hlists.

> {-# LANGUAGE TemplateHaskell #-} -- , RelaxedPolyRec #-}
>
> module Database.HsSqlPpp.Examples.Wrappers.ThHListExample
>     (runTest) where
>
> import Data.List
> {-import Data.HList
> import Data.HList.Label1 ()
> import Data.HList.TypeEqGeneric1 ()
> import Data.HList.TypeCastGeneric1 ()-}

> --import Database.HDBC
>
> --import Database.HsSqlPpp.Utils.DbmsCommon
> import Database.HsSqlPpp.Examples.Wrappers.ConnectionString
> import Database.HsSqlPpp.Examples.Wrappers.ThHListWrapper
>
> runTest :: IO String
> runTest =
>   -- the connection string here is used at runtime
>   withConn ("dbname=" ++ connectionString) $ \conn -> do
>   x <- suppliers conn
>   y <- goodSuppliers conn (Just 4)
>   return ""
>   --return $ "suppliers\n" ++ showl x ++
>   --         "\nsuppliers 4\n" ++ showl y
>   where
>     --showl = intercalate "\n" . map showSup
> type SupplierRecord = Record (HCons (LVPair (Proxy S_no) (Maybe Int))
>                              (HCons (LVPair (Proxy Sname) (Maybe String))
>                              (HCons (LVPair (Proxy Status) (Maybe Int))
>                              (HCons (LVPair (Proxy City) (Maybe String))
>                               HNil))))

 > showSup :: SupplierRecord -> String
 > showSup r = "s_no: " ++ show (r # s_no) ++ ", " ++
 >             "sname: " ++ show (r # sname) ++ ", " ++
 >             "status: " ++ show (r # status) ++ ", " ++
 >             "city: " ++ show (r # city)

> -- the connection string here is used at compile time to get the
> -- type of the sql statement

 > suppliers :: IConnection conn => conn -> IO [(Maybe Int, Maybe String, Maybe Int, Maybe String)]

> suppliers = $(sqlQuery connectionString "select * from s;")

 > goodSuppliers :: IConnection conn => conn
 >               -> Maybe Int
 >               -> IO [(Maybe Int, Maybe String, Maybe Int, Maybe String)]

> goodSuppliers = $(sqlQuery connectionString "select * from s where status >= ?;")
