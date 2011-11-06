> module Database.HsSqlPpp.Tests.TypeChecking.SimpleQueryExprs
>     (simpleQueryExprs) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.TypeChecker


> simpleQueryExprs :: Item
> simpleQueryExprs =
>   Group "simpleQueryExpr"
>   [QueryExpr [CatCreateTable "t" [("a", "int4")
>                                  ,("b", "text")]]
>    "select a,b from t"
>    $ Right $ CompositeType [("a",typeInt)
>                            ,("b", ScalarType "text")]
>
>
>   ,QueryExpr [CatCreateTable "t" [("a", "int4")
>                                  ,("b", "text")]]
>    "select a as c,b as d from t"
>    $ Right $ CompositeType [("c",typeInt)
>                            ,("d", ScalarType "text")]
>
>
>   ,QueryExpr [CatCreateTable "t" [("a", "int4")
>                                  ,("b", "text")]]
>    "select * from t"
>    $ Right $ CompositeType [("a",typeInt)
>                            ,("b", ScalarType "text")]
>   ,QueryExpr [CatCreateTable "t" [("a", "int4")
>                                  ,("b", "text")]]
>    "select t.a,t.b from t"
>    $ Right $ CompositeType [("a",typeInt)
>                            ,("b", ScalarType "text")]

>   ,QueryExpr [CatCreateTable "t" [("a", "int4")
>                                  ,("b", "text")]]
>    "select u.* from t u"
>    $ Right $ CompositeType [("a",typeInt)
>                            ,("b", ScalarType "text")]
>   ,QueryExpr [CatCreateTable "t" [("a", "int4")
>                                  ,("b", "text")]]
>    "select * from t u(c,d)"
>    $ Right $ CompositeType [("c",typeInt)
>                            ,("d", ScalarType "text")]
>   ,QueryExpr [CatCreateTable "t" [("a", "int4")
>                                  ,("b", "text")]]
>    "select u.a,u.b from t u"
>    $ Right $ CompositeType [("a",typeInt)
>                            ,("b", ScalarType "text")]

>
>
>   ,QueryExpr [CatCreateTable "t" [("a", "int4")
>                                  ,("b", "text")]]
>    "select count(*) from t"
>    $ Right $ CompositeType [("count",typeBigInt)]

>   ]
