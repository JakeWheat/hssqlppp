> module Database.HsSqlPpp.Tests.TypeChecking.SimpleQueryExprs
>     (simpleQueryExprs) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Types

> simpleQueryExprs :: Item
> simpleQueryExprs =
>   Group "simpleQueryExpr"
>   [{-QueryExpr [] "select a,b from t"
>    $ Right $ CompositeType [("a",typeInt)
>                            ,("b", ScalarType "text")]-}
>   ]
