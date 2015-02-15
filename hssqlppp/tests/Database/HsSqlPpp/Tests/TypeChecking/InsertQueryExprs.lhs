
> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.InsertQueryExprs
>     (insertQueryExprs) where

> import Database.HsSqlPpp.Tests.TypeChecking.Utils
> import Database.HsSqlPpp.Tests.TestTypes
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.TypeChecker


> insertQueryExprs :: Item
> insertQueryExprs =
>   Group "insertQueryExpr"
>       [
>        -- int to bigint
>        InsertQueryExpr
>         [CatCreateTable "t1" [("a", mkCatNameExtra "int4")]
>         ,CatCreateTable "t2" [("b", mkCatNameExtra "int8")]]
>         "insert into t2(b) select a from t1;"
>         $ Right $ CompositeType [("a", mkTypeExtra typeBigInt)]
>        -- null to not null 
>       ,InsertQueryExpr
>         [CatCreateTable "t1" [("a", mkCatNameExtra "int4")]
>         ,CatCreateTable "t2" [("b", mkCatNameExtraNN "int4")]]
>         "insert into t2(b) select a from t1;"
>         $ Right $ CompositeType [("a", mkTypeExtraNN typeInt)]
>        -- not null to null
>       ,InsertQueryExpr
>         [CatCreateTable "t1" [("a", mkCatNameExtraNN "int4")]
>         ,CatCreateTable "t2" [("b", mkCatNameExtra "int4")]]
>         "insert into t2(b) select a from t1;"
>         $ Right $ CompositeType [("a", mkTypeExtra typeInt)]
>        -- implicit column list
>       ,InsertQueryExpr
>         [CatCreateTable "t1" [("a", mkCatNameExtraNN "int4")]
>         ,CatCreateTable "t2" [("b", mkCatNameExtra "int4")]]
>         "insert into t2 select a from t1;"
>         $ Right $ CompositeType [("a", mkTypeExtra typeInt)]

>        -- star expansion
>        -- one column - todo: why '?column?'?
>       ,InsertQueryExpr
>         [CatCreateTable "t1" [("a", mkCatNameExtraNN "int4")]
>         ,CatCreateTable "t2" [("b", mkCatNameExtraNN "int8")]]
>         "insert into t2 select * from t1;"
>         $ Right $ CompositeType [("?column?", mkTypeExtraNN typeBigInt)]
>        -- two columns (not sure what that correct type is)
>       ,InsertQueryExpr
>         [CatCreateTable "t1" [("a", mkCatNameExtraNN "int4"),("b", mkCatNameExtraNN "int4")]
>         ,CatCreateTable "t2" [("c", mkCatNameExtraNN "int8"),("d", mkCatNameExtraNN "int8")]]
>         "insert into t2 select * from t1;"
>         $ Right $ CompositeType [("?column?", mkTypeExtraNN typeBigInt),("?column?", mkTypeExtraNN typeBigInt)]

>        -- where (uses outerDownEnv)
>       ,InsertQueryExpr
>         [CatCreateTable "t1" [("a", mkCatNameExtraNN "int4")]
>         ,CatCreateTable "t2" [("b", mkCatNameExtraNN "int4")]]
>         "insert into t2 select a from t1 where a>0;"
>         $ Right $ CompositeType [("a", mkTypeExtraNN typeInt)]

>        -- int to bigint - values
>       ,InsertQueryExpr
>         [CatCreateTable "t2" [("b", mkCatNameExtra "int8")]]
>         "insert into t2(b) values (1);"
>         $ Right $ CompositeType [("values%0", mkTypeExtra typeBigInt)]
>        -- not null to null - values
>       ,InsertQueryExpr
>         [CatCreateTable "t2" [("b", mkCatNameExtra "int4")]]
>         "insert into t2(b) values (1);"
>         $ Right $ CompositeType [("values%0", mkTypeExtra typeInt)]
>       ]
