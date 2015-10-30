
Testing schemas in trefs in simple queries

default schema is public - no schema search path
check explicit schemas match
check rewrite to add schemas to syntax if missing

schemas affect typechecking of views and tables only currently

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.TrefSchemas
>     (trefSchemas) where

> --import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Tests.TestTypes
> --import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Types

> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> trefSchemas :: Item
> trefSchemas =
>   Group "trefSchemas"
>       [tcQueryExpr simpleTEnv
>         "select a,b from public.t"
>         $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                                 ,("b", mkTypeExtra $ ScalarType "text")]
>       ,tcQueryExpr simpleTEnv
>         "select a,b from t"
>         $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                                 ,("b", mkTypeExtra $ ScalarType "text")]
>       ,tcQueryExpr simpleTEnv
>         "select a,b from something.t"
>         $ Left $ [UnrecognisedRelation ("something", "t")]

>       ,tcQueryExpr anotherUEnv
>         "select a,b from public.u"
>         $ Left [UnrecognisedRelation ("public", "u")]
>       ,tcQueryExpr anotherUEnv
>         "select a,b from u"
>         $ Left [UnrecognisedRelation ("public", "u")]
>       ,tcQueryExpr anotherUEnv
>         "select a,b from something.u"
>         $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                                 ,("b", mkTypeExtra $ ScalarType "text")]
>       ]
>   where
>     simpleTEnv = [CatCreateTable ("public","t")
>                   [("a", mkCatNameExtra "int4")
>                   ,("b", mkCatNameExtra "text")]]
>     anotherUEnv = [CatCreateTable ("something","u")
>                   [("a", mkCatNameExtra "int4")
>                   ,("b", mkCatNameExtra "text")]]
>     tcQueryExpr cus =
>         let cat = makeCatalog PostgreSQL cus defaultTemplate1Catalog
>         in TCQueryExpr cat defaultTypeCheckFlags
