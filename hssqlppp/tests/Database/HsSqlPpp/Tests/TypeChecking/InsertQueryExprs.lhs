
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
>         [CatCreateTable ("public","t1") [("a", mkCatNameExtra "int4")]
>         ,CatCreateTable ("public","t2") [("b", mkCatNameExtra "int8")]]
>         "insert into t2(b) select a from t1;"
>         $ Right $ CompositeType [("a", mkTypeExtra typeBigInt)]
>        -- null to not null 
>       ,InsertQueryExpr
>         [CatCreateTable ("public","t1") [("a", mkCatNameExtra "int4")]
>         ,CatCreateTable ("public","t2") [("b", mkCatNameExtraNN "int4")]]
>         "insert into t2(b) select a from t1;"
>         $ Right $ CompositeType [("a", mkTypeExtraNN typeInt)]
>        -- not null to null
>       ,InsertQueryExpr
>         [CatCreateTable ("public","t1") [("a", mkCatNameExtraNN "int4")]
>         ,CatCreateTable ("public","t2") [("b", mkCatNameExtra "int4")]]
>         "insert into t2(b) select a from t1;"
>         $ Right $ CompositeType [("a", mkTypeExtra typeInt)]
>        -- implicit column list
>       ,InsertQueryExpr
>         [CatCreateTable ("public","t1") [("a", mkCatNameExtraNN "int4")]
>         ,CatCreateTable ("public","t2") [("b", mkCatNameExtra "int4")]]
>         "insert into t2 select a from t1;"
>         $ Right $ CompositeType [("a", mkTypeExtra typeInt)]

insert + star - fails, commented out for now

        -- star expansion
        -- one column - todo: why '?column?'?
       ,InsertQueryExpr
         [CatCreateTable ("public","t1") [("a", mkCatNameExtraNN "int4")]
         ,CatCreateTable ("public","t2") [("b", mkCatNameExtraNN "int8")]]
         "insert into t2 select * from t1;"
         $ Right $ CompositeType [("a", mkTypeExtraNN typeBigInt)]
        -- two columns (not sure what that correct type is)
       ,InsertQueryExpr
         [CatCreateTable ("public","t1") [("a", mkCatNameExtraNN "int4"),("b", mkCatNameExtraNN "int4")]
         ,CatCreateTable ("public","t2") [("c", mkCatNameExtraNN "int8"),("d", mkCatNameExtraNN "int8")]]
         "insert into t2 select * from t1;"
         $ Right $ CompositeType [("a", mkTypeExtraNN typeBigInt),("b", mkTypeExtraNN typeBigInt)]

>        -- where (uses outerDownEnv)
>       ,InsertQueryExpr
>         [CatCreateTable ("public","t1") [("a", mkCatNameExtraNN "int4")]
>         ,CatCreateTable ("public","t2") [("b", mkCatNameExtraNN "int4")]]
>         "insert into t2 select a from t1 where a>0;"
>         $ Right $ CompositeType [("a", mkTypeExtraNN typeInt)]

>        -- int to bigint - values
>       ,InsertQueryExpr
>         [CatCreateTable ("public","t2") [("b", mkCatNameExtra "int8")]]
>         "insert into t2(b) values (1);"
>         $ Right $ CompositeType [("values%0", mkTypeExtra typeBigInt)]
>        -- not null to null - values
>       ,InsertQueryExpr
>         [CatCreateTable ("public","t2") [("b", mkCatNameExtra "int4")]]
>         "insert into t2(b) values (1);"
>         $ Right $ CompositeType [("values%0", mkTypeExtra typeInt)]
>       ,InsertQueryExpr
>         [CatCreateTable ("public","tt") [("b", mkCatNameExtraNN "int4")]
>         ,CatCreateTable ("public","t") [("d", mkCatNameExtraNN "date")]]
>         "insert into tt select datepart(day,d) from t;"
>        $ Right $ CompositeType [("datepart",mkTypeExtraNN typeInt)]
>       ]
