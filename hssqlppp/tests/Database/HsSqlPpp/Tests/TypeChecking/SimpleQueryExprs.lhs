
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.SimpleQueryExprs
>     (simpleQueryExprs) where

> --import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Tests.TestTypes
> --import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Types

> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> simpleQueryExprs :: Item
> simpleQueryExprs =
>   Group "simpleQueryExpr"
>       [tcQueryExpr
>         [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                             ,("b", mkCatNameExtra "text")]]
>         "select a,b from t"
>         $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                                 ,("b", mkTypeExtra $ ScalarType "text")]
>
>
>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                                      ,("b", mkCatNameExtra "text")]]
>        "select a as c,b as d from t"
>        $ Right $ CompositeType [("c", mkTypeExtra typeInt)
>                                ,("d", mkTypeExtra $ ScalarType "text")]
>
>
>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                                      ,("b", mkCatNameExtra "text")]]
>        "select * from t"
>        $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                                ,("b", mkTypeExtra $ ScalarType "text")]
>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                                      ,("b", mkCatNameExtra "text")]]
>        "select t.a,t.b from t"
>        $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                                ,("b", mkTypeExtra $ ScalarType "text")]

>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                                      ,("b", mkCatNameExtra "text")]]
>        "select u.* from t u"
>        $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                                ,("b", mkTypeExtra $ ScalarType "text")]
>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                                      ,("b", mkCatNameExtra "text")]]
>        "select * from t u(c,d)"
>        $ Right $ CompositeType [("c", mkTypeExtra typeInt)
>                                ,("d", mkTypeExtra $ ScalarType "text")]
>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                                      ,("b", mkCatNameExtra "text")]]
>        "select u.a,u.b from t u"
>        $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                                ,("b", mkTypeExtra $ ScalarType "text")]

>
>
>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")
>                                      ,("b", mkCatNameExtra "text")]]
>        "select count(*) from t"
>        $ Right $ CompositeType [("count", mkTypeExtraNN typeBigInt)]


>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")]
>                  ,CatCreateTable ("public","u") [("a", mkCatNameExtra "int4")]]
>        "select * from t union select * from u"
>        $ Right $ CompositeType [("a", mkTypeExtra typeInt)]

>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")]
>                  ,CatCreateTable ("public","u") [("b", mkCatNameExtra "int4")]]
>        "select * from t union select * from u"
>        $ Right $ CompositeType [("a", mkTypeExtra typeInt)]

todo: implicit casts in union

simple window function type

>       ,tcQueryExpr [CatCreateTable ("public","t") [("a", mkCatNameExtra "int4")]]
>        "select a,count(*) over () as r from t"
>        $ Right $ CompositeType [("a", mkTypeExtra typeInt), ("r", mkTypeExtraNN typeBigInt)]

values

>       ,tcQueryExpr []
>        "values (1)"
>        $ Right $ CompositeType [("values%0", mkTypeExtraNN typeInt)]

>       ,tcQueryExpr []
>        "values (1),(2)"
>        $ Right $ CompositeType [("values%0", mkTypeExtraNN typeInt)]

>       ,tcQueryExpr []
>        "values (1,1.5),(2,2.5)"
>        $ Right $ CompositeType [("values%0", mkTypeExtraNN typeInt), ("values%1", mkTypeExtraNN $ ScalarType "numeric")]

>       ,tcQueryExpr []
>        "values (1.5),(1)"
>        $ Right $ CompositeType [("values%0", mkTypeExtraNN $ ScalarType "numeric")]
>   ]
>   where
>     tcQueryExpr cus =
>         let Right cat = updateCatalog cus defaultTemplate1Catalog
>         in TCQueryExpr cat defaultTypeCheckFlags
