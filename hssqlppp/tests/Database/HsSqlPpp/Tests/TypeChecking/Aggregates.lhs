> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.Aggregates
>     (aggregates) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.TypeChecker

dodgy hack to support cube:
special case the cube

only support case where the group by consists of a single cube or
ordinary groups
no nested groups in the cube
type check
group by cube(a,b,...)
as if it was written
group by a,b,...

TODO: deal with types, null, rollup,cube, grouping sets, nested groups
and flexible combinations

> aggregates :: Item
> aggregates =
>   Group "aggregates"
>   [QueryExpr [CatCreateTable "t" [("a", "int4")
>                                  ,("b", "int4")
>                                  ,("c", "int4")]]
>    "select a,b,count(c) as c from t group by a,b"
>    $ Right $ CompositeType [("a",typeInt)
>                            ,("b",typeInt)
>                            ,("c",typeBigInt)]
>   ,QueryExpr [CatCreateTable "t" [("a", "int4")
>                                  ,("b", "int4")
>                                  ,("c", "int4")]]
>    "select a,b,count(c) as c from t group by cube(a,b)"
>    $ Right $ CompositeType [("a",typeInt)
>                            ,("b",typeInt)
>                            ,("c",typeBigInt)]
>   ]
