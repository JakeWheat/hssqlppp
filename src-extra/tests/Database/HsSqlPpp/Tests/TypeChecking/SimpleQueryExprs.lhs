> module Database.HsSqlPpp.Tests.TypeChecking.SimpleQueryExprs
>     (simpleQueryExprs) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.TypeChecker


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
>
>
>   ,QueryExpr [CatCreateTable "t" [("a", "int4")
>                                  ,("b", "text")]]
>    "select count(*) from t"
>    $ Right $ CompositeType [("count",typeBigInt)]

>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddSelectItemAliases = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select a,b from t"
>    "select a as a,b as b from t"

>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfExpandStars = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select * from t"
>    "select \"a\",\"b\" from t"

>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddFullTablerefAliases = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select * from t"
>    "select * from t as t(a,b)"

>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddQualifiers = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select a,b from t"
>    "select t.a,t.b from t"

>   ,RewriteQueryExpr defaultTypeCheckingFlags {tcfAddQualifiers = True
>                                              ,tcfAddSelectItemAliases = True
>                                              ,tcfExpandStars = True
>                                              ,tcfAddFullTablerefAliases = True}
>          [CatCreateTable "t" [("a", "int4")
>                              ,("b", "text")]]
>    "select * from t"
>    "select t.a as a,t.b as b from t as t(a,b)"

>   ]
