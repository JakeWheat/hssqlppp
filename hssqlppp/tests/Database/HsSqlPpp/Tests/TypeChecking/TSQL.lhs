

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.TSQL
>     (tsqlQueryExprs) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.TypeChecker
> import Data.Text.Lazy (pack)

> tsqlQueryExprs :: Item
> tsqlQueryExprs =
>   Group "tsql" $

datepart, datediff, dateadd

>   [TSQLQueryExpr [CatCreateTable "t" [("a", "date")
>                                      ,("b", "date")]]

this doesn't work:
    "select datediff(hour,a,b) a from t"
todo: fix it

>    "select datediff(hour,a,b) as a from t"
>    $ Right $ CompositeType [("a",typeInt)]
>   ,TSQLQueryExpr [CatCreateTable "t" [("a", "tinyint")
>                                      ,("b", "smallint")]]
>    "select a+b as a from t /* junk it */"
>    $ Right $ CompositeType [("a",typeSmallInt)]]
>   ++ [TSQLQueryExpr [CatCreateTable "t" [("a", "date")]]
>      (pack $ "select datepart(" ++ dp ++ ",a) as a from t")
>      $ Right $ CompositeType [("a",typeInt)]
>      | dp <- ["day","month","year"]]

>   ++ [TSQLQueryExpr [CatCreateTable "t" [("a", "date")]]
>      (pack $ "select dateadd(" ++ dp ++ ",5,a) as a from t")
>      $ Right $ CompositeType [("a",typeDate)]
>      | dp <- ["day","month","year"]]
>   ++ [TSQLQueryExpr [CatCreateTable "t" [("a", "date")]]
>      (pack $ "select dateadd(" ++ dp ++ ",5,'1992-001-01') as a from t")
>      $ Right $ CompositeType [("a",typeDate)]
>      | dp <- ["day","month","year"]]


aggregates: the types of aggregates is different in mssql to
postgresql:

http://msdn.microsoft.com/en-us/library/ms173454.aspx

>   ++ [TSQLQueryExpr [CatCreateTable "t" [("a", inty)]]
>      (pack $ "select " ++ agg ++ "(a) as a from t")
>      $ Right $ CompositeType [("a",resty)]
>      | agg <- ["sum","avg"]

avg,sum

tinyint int
smallint int
int int
bigint bigint
money and smallmoney category ->  money -> todo
float and real category -> float

>      , (inty,resty) <-
>        [("tinyint", typeInt)
>        ,("smallint", typeInt)
>        ,("int", typeInt)
>        ,("int", typeInt)
>        ,("bigint", typeBigInt)
>        ,("float", typeFloat8)
>        ,("real", typeFloat8)
>        ]]

avg

decimal category (p, s) ->
decimal(38, s) divided by decimal(10, 0)

sum

decimal category (p, s) ->
decimal(38, s)

(todo)


count returns int
count_big returns bigint

>   ++
>   [TSQLQueryExpr [CatCreateTable "t" [("a", "int4")]]
>    "select count(*) as a from t"
>    $ Right $ CompositeType [("a",typeInt)]
>   ,TSQLQueryExpr [CatCreateTable "t" [("a", "int4")]]
>    "select count_big(*) as a from t"
>    $ Right $ CompositeType [("a",typeBigInt)]]

todo: add new dialect and stuff for oracle

>   ++
>   [TSQLQueryExpr [CatCreateTable "t" [("a", "timestamp")]]
>    "select trunc(a) as a from t"
>    $ Right $ CompositeType [("a",ScalarType "timestamp")]
>   ,TSQLQueryExpr [CatCreateTable "t" [("a", "date")]]
>    "select trunc(a) as a from t"
>    $ Right $ CompositeType [("a",ScalarType "timestamp")]
>   ,TSQLQueryExpr [CatCreateTable "t" [("a", "date")]]
>    "select a from t where \
>    \ trunc(a) between '2001-01-01 00:00:00' and '2001-04-01 00:00:00'"
>    $ Right $ CompositeType [("a",typeDate)]

>   ,TSQLQueryExpr [CatCreateTable "t" [("a", "int")]]
>    "select decode(a,0,0,1,5,2,6,3,7,10) as a from t"
>    $ Right $ CompositeType [("a",typeInt)]

>   ]
