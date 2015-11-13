

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.TSQL
>     (tsqlQueryExprs) where

> --import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Tests.TestTypes
> --import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.TypeChecker
> import Data.Text.Lazy (pack)
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Dialect
> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> --import Database.HsSqlPpp.Internals.TypesInternal hiding (mkTypeExtra,mkTypeExtraNN)

> tsqlQueryExprs :: Item
> tsqlQueryExprs =
>   Group "tsql" $

datepart, datediff, dateadd

>   [tsqlQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "date")
>                                      ,("b", mkCatNameExtra "date")]]

this doesn't work:
    "select datediff(hour,a,b) a from t"
todo: fix it

>    "select datediff(hour,a,b) as a from t"
>    $ Right $ CompositeType [("a", mkTypeExtra typeInt)]
>   ,tsqlQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "tinyint")
>                                      ,("b", mkCatNameExtra "smallint")]]
>    "select a+b as a from t /* junk it */"
>    $ Right $ CompositeType [("a", mkTypeExtra typeSmallInt)]]
>   ++ [tsqlQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "date")]]
>      (pack $ "select datepart(" ++ dp ++ ",a) as a from t")
>      $ Right $ CompositeType [("a", mkTypeExtra typeInt)]
>      | dp <- ["day","month","year", "hour"]]

>   ++ [tsqlQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "timestamp")]]
>      (pack $ "select datepart(" ++ dp ++ ",a) as a from t")
>      $ Right $ CompositeType [("a", mkTypeExtra typeInt)]
>      | dp <- ["day","month","year", "hour"]]


>   ++ [tsqlQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "date")]]
>      (pack $ "select dateadd(" ++ dp ++ ",5,a) as a from t")
>      $ Right $ CompositeType [("a", mkTypeExtra typeDate)]
>      | dp <- ["day","month","year"]]
>   ++ [tsqlQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "date")]]
>      (pack $ "select dateadd(" ++ dp ++ ",5,'1992-001-01') as a from t")
>      $ Right $ CompositeType [("a", mkTypeExtraNN $ ScalarType "timestamp")]
>      | dp <- ["day","month","year"]]


aggregates: the types of aggregates is different in mssql to
postgresql:

http://msdn.microsoft.com/en-us/library/ms173454.aspx

>   ++ [tsqlQueryExpr [CatCreateTable ("public","t") [("a", inty)]]
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
>        [(mkCatNameExtra "tinyint", mkTypeExtra typeInt)
>        ,(mkCatNameExtra "smallint", mkTypeExtra typeInt)
>        ,(mkCatNameExtra "int", mkTypeExtra typeInt)
>        --,(mkCatNameExtra "int", mkTypeExtra typeInt)
>        ,(mkCatNameExtra "bigint", mkTypeExtra typeBigInt)
>        ,(mkCatNameExtra "float", mkTypeExtra typeFloat8)
>        ,(mkCatNameExtra "real", mkTypeExtra typeFloat8)
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
>   [tsqlQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")]]
>    "select count(*) as a from t"
>    $ Right $ CompositeType [("a", mkTypeExtraNN typeInt)]
>   ,tsqlQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")]]
>    "select count_big(*) as a from t"
>    $ Right $ CompositeType [("a", mkTypeExtraNN typeBigInt)]]

todo: add new dialect and stuff for oracle

>   ++
>   [tsqlQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "timestamp")]]
>    "select trunc(a) as a from t"
>    $ Right $ CompositeType [("a", mkTypeExtra $ ScalarType "timestamp")]
>   ,tsqlQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "date")]]
>    "select trunc(a) as a from t"
>    $ Right $ CompositeType [("a", mkTypeExtra $ ScalarType "timestamp")]
>   ,tsqlQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "date")]]
>    "select a from t where \
>    \ trunc(a) between '2001-01-01 00:00:00' and '2001-04-01 00:00:00'"
>    $ Right $ CompositeType [("a", mkTypeExtra typeDate)]

>   ,tsqlQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int")]]
>    "select decode(a,0,0,1,5,2,6,3,7,10) as a from t"
>    $ Right $ CompositeType [("a", mkTypeExtra typeInt)]

>   ]
>   where
>     tsqlQueryExpr cus =
>         let cat = makeCatalog sqlServerDialect cus
>         in TCQueryExpr cat defaultTypeCheckFlags {tcfDialect = sqlServerDialect}
>     typeInt = ScalarType "int4"
>     typeSmallInt = ScalarType "int2"
>     typeDate = ScalarType "date"
>     typeBigInt = ScalarType "int8"
>     typeFloat8 = ScalarType "float8"
