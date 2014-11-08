

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.TrefIdentifiers
>     (trefIdentifiers) where

> --import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Tests.TestTypes
> --import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Types
> import Database.HsSqlPpp.Tests.TypeChecking.Utils


> trefIdentifiers :: Item
> trefIdentifiers =
>   Group "trefIdentifiers"
>   [qenc "select * from t0 cross join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                               ,("b", mkTypeExtra $ ScalarType "text")
>                               ,("c", mkTypeExtra typeInt)
>                               ,("d", mkTypeExtra $ ScalarType "text")]
>   ,qenc "select t0.* from t0 cross join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                               ,("b", mkTypeExtra $ ScalarType "text")]
>   ,qenc "select t1.* from t0 cross join t1"
>       $ Right $ CompositeType [("c", mkTypeExtra typeInt)
>                               ,("d", mkTypeExtra $ ScalarType "text")]
>   ,qenc "select *, t0.*, t1.* from t0 cross join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                               ,("b", mkTypeExtra $ ScalarType "text")
>                               ,("c", mkTypeExtra typeInt)
>                               ,("d", mkTypeExtra $ ScalarType "text")
>                               ,("a", mkTypeExtra typeInt)
>                               ,("b", mkTypeExtra $ ScalarType "text")
>                               ,("c", mkTypeExtra typeInt)
>                               ,("d", mkTypeExtra $ ScalarType "text")]

>   ,qec "select * from t0 cross join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                               ,("b", mkTypeExtra $ ScalarType "text")
>                               ,("a", mkTypeExtra typeInt)
>                               ,("c", mkTypeExtra $ ScalarType "text")]
>   ,qec "select t0.* from t0 cross join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                               ,("b", mkTypeExtra $ ScalarType "text")]
>   ,qec "select t1.* from t0 cross join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                               ,("c", mkTypeExtra $ ScalarType "text")]
>   ,qec "select *, t0.*, t1.* from t0 cross join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                               ,("b", mkTypeExtra $ ScalarType "text")
>                               ,("a", mkTypeExtra typeInt)
>                               ,("c", mkTypeExtra $ ScalarType "text")
>                               ,("a", mkTypeExtra typeInt)
>                               ,("b", mkTypeExtra $ ScalarType "text")
>                               ,("a", mkTypeExtra typeInt)
>                               ,("c", mkTypeExtra $ ScalarType "text")]
>   ,qec "select t0.a from t0 cross join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)]
>   ,qec "select t1.a from t0 cross join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)]
>   ,qec "select a from t0 cross join t1"
>       $ Left [AmbiguousIdentifier "a"]

>   ,qec "select * from t0 natural inner join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                               ,("b", mkTypeExtra $ ScalarType "text")
>                               ,("c", mkTypeExtra $ ScalarType "text")]
>   ,qec "select t0.* from t0 natural inner join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                               ,("b", mkTypeExtra $ ScalarType "text")]
>   ,qec "select t1.* from t0 natural inner join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                               ,("c", mkTypeExtra $ ScalarType "text")]
>   ,qec "select *, t0.*, t1.* from t0 natural inner join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)
>                               ,("b", mkTypeExtra $ ScalarType "text")
>                               ,("c", mkTypeExtra $ ScalarType "text")
>                               ,("a", mkTypeExtra typeInt)
>                               ,("b", mkTypeExtra $ ScalarType "text")
>                               ,("a", mkTypeExtra typeInt)
>                               ,("c", mkTypeExtra $ ScalarType "text")]
>   ,qec "select t0.a from t0 natural inner join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)]
>   ,qec "select t1.a from t0 natural inner join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)]
>   ,qec "select a from t0 natural inner join t1"
>       $ Right $ CompositeType [("a", mkTypeExtra typeInt)]



>   ]
>   where
>     qenc = TCQueryExpr [CatCreateTable "t0" [("a", mkCatNameExtra "int4")
>                                           ,("b", mkCatNameExtra "text")]
>                      ,CatCreateTable "t1" [("c", mkCatNameExtra "int4")
>                                           ,("d", mkCatNameExtra "text")]]
>     qec = TCQueryExpr [CatCreateTable "t0" [("a", mkCatNameExtra "int4")
>                                          ,("b", mkCatNameExtra "text")]
>                     ,CatCreateTable "t1" [("a", mkCatNameExtra "int4")
>                                          ,("c", mkCatNameExtra "text")]]



todo:
unrecognised id
bogus qualifier for real id
